Kernel code --

/*
 * Copyright 1993-2009 NVIDIA Corporation.  All rights reserved.
 *
 * NVIDIA Corporation and its licensors retain all intellectual property and 
 * proprietary rights in and to this software and related documentation. 
 * Any use, reproduction, disclosure, or distribution of this software 
 * and related documentation without an express license agreement from
 * NVIDIA Corporation is strictly prohibited.
 *
 * Please refer to the applicable NVIDIA end user license agreement (EULA) 
 * associated with this source code for terms and conditions that govern 
 * your use of this NVIDIA software.
 * 
 */

/*
    Parallel reduction kernels
*/

// The following defines are set during runtime compilation, see reduction.cpp
// #define T float
// #define blockSize 128
// #define nIsPow2 1

#ifndef _REDUCE_KERNEL_H_
#define _REDUCE_KERNEL_H_

/*
    Parallel sum reduction using shared memory
    - takes log(n) steps for n input elements
    - uses n threads
    - only works for power-of-2 arrays
*/

/* This reduction interleaves which threads are active by using the modulo
   operator.  This operator is very expensive on GPUs, and the interleaved 
   inactivity means that no whole warps are active, which is also very 
   inefficient */

__kernel void reduce0(__global T *g_idata, __global T *g_odata, unsigned int n, __local T* sdata)
{
    // load shared mem
    unsigned int tid = get_local_id(0);
    unsigned int i = get_global_id(0);
    
    sdata[tid] = (i < n) ? g_idata[i] : 0;
    
    barrier(CLK_LOCAL_MEM_FENCE);

    // do reduction in shared mem
    for(unsigned int s=1; s < get_local_size(0); s *= 2) {
        // modulo arithmetic is slow!
        if ((tid % (2*s)) == 0) {
            sdata[tid] += sdata[tid + s];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }

    // write result for this block to global mem
    if (tid == 0) g_odata[get_group_id(0)] = sdata[0];
}


/* This version uses contiguous threads, but its interleaved 
   addressing results in many shared memory bank conflicts. */
__kernel void reduce1(__global T *g_idata, __global T *g_odata, unsigned int n, __local T* sdata)
{
    // load shared mem
    unsigned int tid = get_local_id(0);
    unsigned int i = get_global_id(0);
    
    sdata[tid] = (i < n) ? g_idata[i] : 0;
    
    barrier(CLK_LOCAL_MEM_FENCE);

    // do reduction in shared mem
    for(unsigned int s=1; s < get_local_size(0); s *= 2) 
    {
        int index = 2 * s * tid;

        if (index < get_local_size(0)) 
        {
            sdata[index] += sdata[index + s];
        }

        barrier(CLK_LOCAL_MEM_FENCE);
    }

    // write result for this block to global mem
    if (tid == 0) g_odata[get_group_id(0)] = sdata[0];
}

/*
    This version uses sequential addressing -- no divergence or bank conflicts.
*/
__kernel void reduce2(__global T *g_idata, __global T *g_odata, unsigned int n, __local T* sdata)
{
    // load shared mem
    unsigned int tid = get_local_id(0);
    unsigned int i = get_global_id(0);
    
    sdata[tid] = (i < n) ? g_idata[i] : 0;
    
    barrier(CLK_LOCAL_MEM_FENCE);

    // do reduction in shared mem
    for(unsigned int s=get_local_size(0)/2; s>0; s>>=1) 
    {
        if (tid < s) 
        {
            sdata[tid] += sdata[tid + s];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }

    // write result for this block to global mem
    if (tid == 0) g_odata[get_local_size(0)] = sdata[0];
}

/*
    This version uses n/2 threads --
    it performs the first level of reduction when reading from global memory
*/
__kernel void reduce3(__global T *g_idata, __global T *g_odata, unsigned int n, __local T* sdata)
{
    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = get_local_id(0);
    unsigned int i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);

    sdata[tid] = (i < n) ? g_idata[i] : 0;
    if (i + get_local_size(0) < n) 
        sdata[tid] += g_idata[i+get_local_size(0)];  

    barrier(CLK_LOCAL_MEM_FENCE);

    // do reduction in shared mem
    for(unsigned int s=get_local_size(0)/2; s>0; s>>=1) 
    {
        if (tid < s) 
        {
            sdata[tid] += sdata[tid + s];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }

    // write result for this block to global mem 
    if (tid == 0) g_odata[get_group_id(0)] = sdata[0];
}

/*
    This version unrolls the last warp to avoid synchronization where it 
    isn't needed
*/
__kernel void reduce4(__global T *g_idata, __global T *g_odata, unsigned int n, __local T* sdata)
{
    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = get_local_id(0);
    unsigned int i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);

    sdata[tid] = (i < n) ? g_idata[i] : 0;
    if (i + get_local_size(0) < n) 
        sdata[tid] += g_idata[i+get_local_size(0)];  

    barrier(CLK_LOCAL_MEM_FENCE);

    // do reduction in shared mem
    #pragma unroll 1
    for(unsigned int s=get_local_size(0)/2; s>32; s>>=1) 
    {
        if (tid < s) 
        {
            sdata[tid] += sdata[tid + s];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }

    if (tid < 32)
    {
        if (blockSize >=  64) { sdata[tid] += sdata[tid + 32]; }
        if (blockSize >=  32) { sdata[tid] += sdata[tid + 16]; }
        if (blockSize >=  16) { sdata[tid] += sdata[tid +  8]; }
        if (blockSize >=   8) { sdata[tid] += sdata[tid +  4]; }
        if (blockSize >=   4) { sdata[tid] += sdata[tid +  2]; }
        if (blockSize >=   2) { sdata[tid] += sdata[tid +  1]; }
    }

    // write result for this block to global mem 
    if (tid == 0) g_odata[get_group_id(0)] = sdata[0];
}

/*
    This version is completely unrolled.  It uses a template parameter to achieve 
    optimal code for any (power of 2) number of threads.  This requires a switch 
    statement in the host code to handle all the different thread block sizes at 
    compile time.
*/
__kernel void reduce5(__global T *g_idata, __global T *g_odata, unsigned int n, __local T* sdata)
{
    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = get_local_id(0);
    unsigned int i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);

    sdata[tid] = (i < n) ? g_idata[i] : 0;
    if (i + blockSize < n) 
        sdata[tid] += g_idata[i+blockSize];  

    barrier(CLK_LOCAL_MEM_FENCE);

    // do reduction in shared mem
    if (blockSize >= 512) { if (tid < 256) { sdata[tid] += sdata[tid + 256]; } barrier(CLK_LOCAL_MEM_FENCE); }
    if (blockSize >= 256) { if (tid < 128) { sdata[tid] += sdata[tid + 128]; } barrier(CLK_LOCAL_MEM_FENCE); }
    if (blockSize >= 128) { if (tid <  64) { sdata[tid] += sdata[tid +  64]; } barrier(CLK_LOCAL_MEM_FENCE); }
    
    if (tid < 32)
    {
        if (blockSize >=  64) { sdata[tid] += sdata[tid + 32]; }
        if (blockSize >=  32) { sdata[tid] += sdata[tid + 16]; }
        if (blockSize >=  16) { sdata[tid] += sdata[tid +  8]; }
        if (blockSize >=   8) { sdata[tid] += sdata[tid +  4]; }
        if (blockSize >=   4) { sdata[tid] += sdata[tid +  2]; }
        if (blockSize >=   2) { sdata[tid] += sdata[tid +  1]; }
    }
    
    // write result for this block to global mem 
    if (tid == 0) g_odata[get_group_id(0)] = sdata[0];
}

/*
    This version adds multiple elements per thread sequentially.  This reduces the overall
    cost of the algorithm while keeping the work complexity O(n) and the step complexity O(log n).
    (Brent's Theorem optimization)
*/
__kernel void reduce6(__global T *g_idata, __global T *g_odata, unsigned int n, __local T* sdata)
{
    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = get_local_id(0);
    unsigned int i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);
    unsigned int gridSize = blockSize*2*get_num_groups(0);
    sdata[tid] = 0;

    // we reduce multiple elements per thread.  The number is determined by the 
    // number of active thread blocks (via gridDim).  More blocks will result
    // in a larger gridSize and therefore fewer elements per thread
    while (i < n)
    {         
        sdata[tid] += g_idata[i];
        // ensure we don't read out of bounds -- this is optimized away for powerOf2 sized arrays
        if (nIsPow2 || i + blockSize < n) 
            sdata[tid] += g_idata[i+blockSize];  
        i += gridSize;
    } 

    barrier(CLK_LOCAL_MEM_FENCE);

    // do reduction in shared mem
    if (blockSize >= 512) { if (tid < 256) { sdata[tid] += sdata[tid + 256]; } barrier(CLK_LOCAL_MEM_FENCE); }
    if (blockSize >= 256) { if (tid < 128) { sdata[tid] += sdata[tid + 128]; } barrier(CLK_LOCAL_MEM_FENCE); }
    if (blockSize >= 128) { if (tid <  64) { sdata[tid] += sdata[tid +  64]; } barrier(CLK_LOCAL_MEM_FENCE); }
    
    if (tid < 32)
    {
        if (blockSize >=  64) { sdata[tid] += sdata[tid + 32]; }
        if (blockSize >=  32) { sdata[tid] += sdata[tid + 16]; }
        if (blockSize >=  16) { sdata[tid] += sdata[tid +  8]; }
        if (blockSize >=   8) { sdata[tid] += sdata[tid +  4]; }
        if (blockSize >=   4) { sdata[tid] += sdata[tid +  2]; }
        if (blockSize >=   2) { sdata[tid] += sdata[tid +  1]; }
    }
    
    // write result for this block to global mem 
    if (tid == 0) g_odata[get_group_id(0)] = sdata[0];
}

#endif // #ifndef _REDUCE_KERNEL_H_


//---------------------------------------End of Kernel Code----------------------------------------------------------------  

//Scala Code -- 

object ReductionKernel {

  def main(args: Array[String]) = {
    val a = Array[Float](2.4f, 6.7f, 3.9f,23.53f)
    val b = Array[Float](14.12f, 23.55f, 34.6f, 12.1f)
    var c = Array[Float]
    var d = Array[Float]
    var e = Array[Float]
    var f = Array[Float]
    var g = Array[Float]
    var h = Array[Float]
    val i = UInt(4)
    

    c = GPU_reduce0(a, b, i)
    d = GPU_reduce1(a, b, i)
    e = GPU_reduce2(a, b, i)
    f = GPU_reduce3(a, b, i)
    g = GPU_reduce4(a, b, i)
    h = GPU_reduce5(a, b, i)
    i = GPU_reduce6(a, b, i)
    

    c.foreach(i => println(i))
    d.foreach(i => println(i))
    e.foreach(i => println(i))
    f.foreach(i => println(i))
    g.foreach(i => println(i))
    h.foreach(i => println(i))
    i.foreach(i => println(i))
    
  }

def GPU_reduce0(idata: GlobalFloatP, odata: GlobalFloatP, UInt n, sdata: GlobalFloatP): Array[Float] = {

val tid=getLocalId(0)
val i=getGlocalId(0)

sdata(tid) = if (i<n) idata(i) else 0

localMem.barrier

for( i <- 1 until getLocalSize(0)){

if(tid % ( 2 * i ) == 0 ) 
sdata(tid) += sdata(tid+s)

localMem.barrier
s*=2
}

if(tid==0) odata(getGroupId(0))=sdata(0)

 }
 
def GPU_reduce1(idata: GlobalFloatP, odata: GlobalFloatP, UInt n, sdata: GlobalFloatP): Array[Float] = {
 
 val tid=getLocalId(0)
 vaal i=getGlocalId(0)
 
 sdata(tid) = if (i<n) idata(i) else 0
 
 localMem.barrier
 
 for( i <- 1 until getLocalSize(0)){
 
 val index= 2 * s * tid
 
 if(index < getLocalSize(0) ) 
 sdata(index) += sdata(index+s)
 localMem.barrier
 s*=2
 }
 
 if(tid==0) odata(getGroupId(0))=sdata(0)
 
 }
 
  def GPU_reduce2(idata: GlobalFloatP, odata: GlobalFloatP, UInt n, sdata: GlobalFloatP): Array[Float] = {
  
  val tid=getLocalId(0)
  val i=getGlocalId(0)
  
  sdata(tid) = if (i<n) idata(i) else 0
  
  localMem.barrier
  
  for( i <- 1 until getLocalSize(0)){
  
  val index= 2 * s * tid
  
  if(index < getLocalSize(0) ) 
  sdata(index) += sdata(index+s)
  localMem.barrier
  s*=2
  }
  
  if(tid==0) odata(getGroupId(0))=sdata(0)
  
 }
 
   def GPU_reduce3(idata: GlobalFloatP, odata: GlobalFloatP, UInt n, sdata: GlobalFloatP): Array[Float] = {
   
   val tid=getLocalId(0)
   val i=getGlocalId(0) * getLocalSize(0)*2 + getLocalId(0)
   
   sdata(tid) = if (i<n) idata(i) else 0
   if ( i + getLocaId(0) < n) 
      sdata(tid) += idata(i+getLocalSize(0))
      
   localMem.barrier
   
   val i=UInt
   
   for( i <- getLocalSize(0)/2 until 0 )
   {
    
   if(tid < s )
   sdata(tid) += sdata(tid+s)
   
   localMem.barrier
   s>>=1
   }
   
   if(tid==0) odata(getGroupId(0))=sdata(0)
   
 }
 
   def GPU_reduce4(idata: GlobalFloatP, odata: GlobalFloatP, UInt n, sdata: GlobalFloatP): Array[Float] = {
    
    val tid=getLocalId(0)
    val i=getGlocalId(0) * getLocalSize(0)*2 + getLocalId(0)
    
    sdata(tid) = if (i<n) idata(i) else 0
    if ( i + getLocaId(0) < n) 
       sdata(tid) += idata(i+getLocalSize(0))
       
    localMem.barrier
    
    val i=UInt
    pragma.unroll(1)
    for( i <- getLocalSize(0)/2 until s > 32 )
    {
     
    if(tid < s )
    sdata(tid) += sdata(tid+s)
    
    localMem.barrier
    s>>=1
    }
    
     if (tid < 32)
        {
            if (getBlockSize >=  64) sdata(tid) += sdata(tid + 32)
            if (getBlockSize >=  32) sdata(tid) += sdata(tid + 16)
            if (getBlockSize >=  16) sdata(tid) += sdata(tid + 8)
            if (getBlockSize >=   8) sdata(tid) += sdata(tid + 4)
            if (getBlockSize >=   4) sdata(tid) += sdata(tid + 2)
            if (getBlockSize >=   2) sdata(tid) += sdata(tid + 1)
    }
    
    if(tid==0) odata(getGroupId(0))=sdata(0)
    
 }
 
   def GPU_reduce5(idata: GlobalFloatP, odata: GlobalFloatP, UInt n, sdata: GlobalFloatP): Array[Float] = {
     
     val tid=getLocalId(0)
     val i=getGlocalId(0) * getLocalSize(0)*2 + getLocalId(0)
     
     sdata(tid) = if (i<n) idata(i) else 0
     if ( i + getBlockSize < n) 
        sdata(tid) += idata(i+getBlockSize)
        
     localMem.barrier
     
     
     if (getBlockSize >= 512) { if (tid < 256) sdata(tid) += sdata(tid + 256); localMem.barrier }
     if (getBlockSize >= 256) { if (tid < 128) sdata[tid] += sdata[tid + 128]; localMem.barrier }
     if (getBlockSize >= 128) { if (tid <  64) sdata[tid] += sdata[tid +  64]; localMem.barrier }
     
      if (tid < 32)
         {
             if (getBlockSize >=  64) sdata(tid) += sdata(tid + 32)
             if (getBlockSize >=  32) sdata(tid) += sdata(tid + 16)
             if (getBlockSize >=  16) sdata(tid) += sdata(tid + 8)
             if (getBlockSize >=   8) sdata(tid) += sdata(tid + 4)
             if (getBlockSize >=   4) sdata(tid) += sdata(tid + 2)
             if (getBlockSize >=   2) sdata(tid) += sdata(tid + 1)
     }
     
     if(tid==0) odata(getGroupId(0))=sdata(0)
     
 }
 
    def GPU_reduce6(idata: GlobalFloatP, odata: GlobalFloatP, UInt n, sdata: GlobalFloatP): Array[Float] = {
      
      val tid=getLocalId(0)
      val i=getGlocalId(0) * getLocalSize(0)*2 + getLocalId(0)
      val gridSize = getBlockSize * 2 * getNumGroups(0)
      sdata(tid)=0
      
      while(i<n){
      
      sdata(tid)+= idata(i)
      if( isPow2(n) || i + getBlockSize < n )
          sdata(tid) += idata(t+getBlockSize)
      i+= gridSize
      
      }
      
      localMem.barrier
      
      if (getBlockSize >= 512) { if (tid < 256) sdata(tid) += sdata(tid + 256); localMem.barrier }
      if (getBlockSize >= 256) { if (tid < 128) sdata[tid] += sdata[tid + 128]; localMem.barrier }
      if (getBlockSize >= 128) { if (tid <  64) sdata[tid] += sdata[tid +  64]; localMem.barrier }
      
       if (tid < 32)
          {
              if (getBlockSize >=  64) sdata(tid) += sdata(tid + 32)
              if (getBlockSize >=  32) sdata(tid) += sdata(tid + 16)
              if (getBlockSize >=  16) sdata(tid) += sdata(tid + 8)
              if (getBlockSize >=   8) sdata(tid) += sdata(tid + 4)
              if (getBlockSize >=   4) sdata(tid) += sdata(tid + 2)
              if (getBlockSize >=   2) sdata(tid) += sdata(tid + 1)
      }
      
      if(tid==0) odata(getGroupId(0))=sdata(0)
      
  }

 }