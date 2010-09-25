//Kernel code

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

#define LOCAL_SIZE_LIMIT 1024U

inline void ComparatorPrivate(
    uint *keyA,
    uint *valA,
    uint *keyB,
    uint *valB,
    uint dir
){
    if( (*keyA > *keyB) == dir ){
        uint t;
        t = *keyA; *keyA = *keyB; *keyB = t;
        t = *valA; *valA = *valB; *valB = t;
    }
}

inline void ComparatorLocal(
    __local uint *keyA,
    __local uint *valA,
    __local uint *keyB,
    __local uint *valB,
    uint dir
){
    if( (*keyA > *keyB) == dir ){
        uint t;
        t = *keyA; *keyA = *keyB; *keyB = t;
        t = *valA; *valA = *valB; *valB = t;
    }
}

////////////////////////////////////////////////////////////////////////////////
// Monolithic bitonic sort kernel for short arrays fitting into local memory
////////////////////////////////////////////////////////////////////////////////
__kernel void bitonicSortLocal(
    __global uint *d_DstKey,
    __global uint *d_DstVal,
    __global uint *d_SrcKey,
    __global uint *d_SrcVal,
    uint arrayLength,
    uint dir
){
    __local  uint l_key[LOCAL_SIZE_LIMIT];
    __local  uint l_val[LOCAL_SIZE_LIMIT];

    //Offset to the beginning of subbatch and load data
    d_SrcKey += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    d_SrcVal += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    d_DstKey += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    d_DstVal += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    l_key[get_local_id(0) +                      0] = d_SrcKey[                     0];
    l_val[get_local_id(0) +                      0] = d_SrcVal[                     0];
    l_key[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)] = d_SrcKey[(LOCAL_SIZE_LIMIT / 2)];
    l_val[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)] = d_SrcVal[(LOCAL_SIZE_LIMIT / 2)];

    for(uint size = 2; size < arrayLength; size <<= 1){
        //Bitonic merge
        uint ddd = dir ^ ( (get_local_id(0) & (size / 2)) != 0 );
        for(uint stride = size / 2; stride > 0; stride >>= 1){
            barrier(CLK_LOCAL_MEM_FENCE);
            uint pos = 2 * get_local_id(0) - (get_local_id(0) & (stride - 1));
            ComparatorLocal(
                &l_key[pos +      0], &l_val[pos +      0],
                &l_key[pos + stride], &l_val[pos + stride],
                ddd
            );
        }
    }

    //ddd == dir for the last bitonic merge step
    {
        for(uint stride = arrayLength / 2; stride > 0; stride >>= 1){
            barrier(CLK_LOCAL_MEM_FENCE);
            uint pos = 2 * get_local_id(0) - (get_local_id(0) & (stride - 1));
            ComparatorLocal(
                &l_key[pos +      0], &l_val[pos +      0],
                &l_key[pos + stride], &l_val[pos + stride],
                dir
            );
        }
    }

    barrier(CLK_LOCAL_MEM_FENCE);
    d_DstKey[                     0] = l_key[get_local_id(0) +                      0];
    d_DstVal[                     0] = l_val[get_local_id(0) +                      0];
    d_DstKey[(LOCAL_SIZE_LIMIT / 2)] = l_key[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)];
    d_DstVal[(LOCAL_SIZE_LIMIT / 2)] = l_val[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)];
}

////////////////////////////////////////////////////////////////////////////////
// Bitonic sort kernel for large arrays (not fitting into local memory)
////////////////////////////////////////////////////////////////////////////////
//Bottom-level bitonic sort
//Almost the same as bitonicSortLocal with the only exception
//of even / odd subarrays (of LOCAL_SIZE_LIMIT points) being
//sorted in opposite directions
__kernel void bitonicSortLocal1(
    __global uint *d_DstKey,
    __global uint *d_DstVal,
    __global uint *d_SrcKey,
    __global uint *d_SrcVal
){
    __local uint l_key[LOCAL_SIZE_LIMIT];
    __local uint l_val[LOCAL_SIZE_LIMIT];

    //Offset to the beginning of subarray and load data
    d_SrcKey += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    d_SrcVal += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    d_DstKey += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    d_DstVal += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    l_key[get_local_id(0) +                      0] = d_SrcKey[                     0];
    l_val[get_local_id(0) +                      0] = d_SrcVal[                     0];
    l_key[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)] = d_SrcKey[(LOCAL_SIZE_LIMIT / 2)];
    l_val[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)] = d_SrcVal[(LOCAL_SIZE_LIMIT / 2)];

    uint comparatorI = get_global_id(0) & ((LOCAL_SIZE_LIMIT / 2) - 1);

    for(uint size = 2; size < LOCAL_SIZE_LIMIT; size <<= 1){
        //Bitonic merge
        uint ddd = (comparatorI & (size / 2)) != 0;
        for(uint stride = size / 2; stride > 0; stride >>= 1){
            barrier(CLK_LOCAL_MEM_FENCE);
            uint pos = 2 * get_local_id(0) - (get_local_id(0) & (stride - 1));
            ComparatorLocal(
                &l_key[pos +      0], &l_val[pos +      0],
                &l_key[pos + stride], &l_val[pos + stride],
                ddd
            );
        }
    }

    //Odd / even arrays of LOCAL_SIZE_LIMIT elements
    //sorted in opposite directions
    {
        uint ddd = (get_group_id(0) & 1);
        for(uint stride = LOCAL_SIZE_LIMIT / 2; stride > 0; stride >>= 1){
            barrier(CLK_LOCAL_MEM_FENCE);
            uint pos = 2 * get_local_id(0) - (get_local_id(0) & (stride - 1));
            ComparatorLocal(
                &l_key[pos +      0], &l_val[pos +      0],
                &l_key[pos + stride], &l_val[pos + stride],
               ddd
            );
        }
    }

    barrier(CLK_LOCAL_MEM_FENCE);
    d_DstKey[                     0] = l_key[get_local_id(0) +                      0];
    d_DstVal[                     0] = l_val[get_local_id(0) +                      0];
    d_DstKey[(LOCAL_SIZE_LIMIT / 2)] = l_key[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)];
    d_DstVal[(LOCAL_SIZE_LIMIT / 2)] = l_val[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)];
}

//Bitonic merge iteration for 'stride' >= LOCAL_SIZE_LIMIT
__kernel void bitonicMergeGlobal(
    __global uint *d_DstKey,
    __global uint *d_DstVal,
    __global uint *d_SrcKey,
    __global uint *d_SrcVal,
    uint arrayLength,
    uint size,
    uint stride,
    uint dir
){
    uint global_comparatorI = get_global_id(0);
    uint        comparatorI = global_comparatorI & (arrayLength / 2 - 1);

    //Bitonic merge
    uint ddd = dir ^ ( (comparatorI & (size / 2)) != 0 );
    uint pos = 2 * global_comparatorI - (global_comparatorI & (stride - 1));

    uint keyA = d_SrcKey[pos +      0];
    uint valA = d_SrcVal[pos +      0];
    uint keyB = d_SrcKey[pos + stride];
    uint valB = d_SrcVal[pos + stride];

    ComparatorPrivate(
        &keyA, &valA,
        &keyB, &valB,
        ddd
    );

    d_DstKey[pos +      0] = keyA;
    d_DstVal[pos +      0] = valA;
    d_DstKey[pos + stride] = keyB;
    d_DstVal[pos + stride] = valB;
}

//Combined bitonic merge steps for
//'size' > LOCAL_SIZE_LIMIT and 'stride' = [1 .. LOCAL_SIZE_LIMIT / 2]
__kernel void bitonicMergeLocal(
    __global uint *d_DstKey,
    __global uint *d_DstVal,
    __global uint *d_SrcKey,
    __global uint *d_SrcVal,
    uint arrayLength,
    uint stride,
    uint size,
    uint dir
){
    __local uint l_key[LOCAL_SIZE_LIMIT];
    __local uint l_val[LOCAL_SIZE_LIMIT];

    d_SrcKey += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    d_SrcVal += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    d_DstKey += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    d_DstVal += get_group_id(0) * LOCAL_SIZE_LIMIT + get_local_id(0);
    l_key[get_local_id(0) +                      0] = d_SrcKey[                     0];
    l_val[get_local_id(0) +                      0] = d_SrcVal[                     0];
    l_key[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)] = d_SrcKey[(LOCAL_SIZE_LIMIT / 2)];
    l_val[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)] = d_SrcVal[(LOCAL_SIZE_LIMIT / 2)];

    //Bitonic merge
    uint comparatorI = get_global_id(0) & ((arrayLength / 2) - 1);
    uint         ddd = dir ^ ( (comparatorI & (size / 2)) != 0 );
    for(; stride > 0; stride >>= 1){
        barrier(CLK_LOCAL_MEM_FENCE);
        uint pos = 2 * get_local_id(0) - (get_local_id(0) & (stride - 1));
        ComparatorLocal(
            &l_key[pos +      0], &l_val[pos +      0],
            &l_key[pos + stride], &l_val[pos + stride],
            ddd
        );
    }

    barrier(CLK_LOCAL_MEM_FENCE);
    d_DstKey[                     0] = l_key[get_local_id(0) +                      0];
    d_DstVal[                     0] = l_val[get_local_id(0) +                      0];
    d_DstKey[(LOCAL_SIZE_LIMIT / 2)] = l_key[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)];
    d_DstVal[(LOCAL_SIZE_LIMIT / 2)] = l_val[get_local_id(0) + (LOCAL_SIZE_LIMIT / 2)];
}

*/

//---------------------------------------End of Kernel Code----------------------------------------------------------------  

//Scala Code -- 

object BitonicSort {

def LOCAL_SIZE_LIMIT=1024U

implicit ComparatorPrivate(keyA: UInt, valA: UInt, keyB: UInt, valB: UInt, dir: UInt){
    if( (keyA.d > keyB.d) == dir ){
        uint t;
        t = keyA.d; keyA.d = keyB.d; keyB.d = t;
        t = valA.d; valA.d = valB.d; valB.d = t;
    }
}

implicit ComparatorLocal(keyA: LocalUIntP, valA: LocalUIntP ,keyB: LocalUIntP,valB: LocalUIntP,dir: UInt) ={
      if( (keyA.d > keyB.d) == dir ){
           uint t;
           t = keyA.d; keyA.d = keyB.d; keyB.d = t;
           t = valA.d; valA.d = valB.d; valB.d = t;
    }
}

def GPU_bitonicSortLocal(d_DstKey: GlobalUIntP, d_DstVal: GlobalUIntP, d_SrcKey: GlobalUIntP, d_SrcVal: GlobalUIntP,
    uint arrayLength: UInt, dir: UInt) ={

    var l_key = new Array[LocalUInt](LOCAL_SIZE_LIMIT)
    var l_val = new Array[LocalUInt](LOCAL_SIZE_LIMIT)

    //Offset to the beginning of subbatch and load data
    d_SrcKey += getGroupId(0) * LOCAL_SIZE_LIMIT + getLocalId(0)
    d_SrcVal += getGroupId(0) * LOCAL_SIZE_LIMIT + getLocalId(0)
    d_DstKey += getGroupId(0) * LOCAL_SIZE_LIMIT + getLocalId(0)
    d_DstVal += getGroupId(0) * LOCAL_SIZE_LIMIT + getLocalId(0)
    l_key(getLocalId(0) +                      0) = d_SrcKey(                     0)
    l_val(getLocalId(0) +                      0) = d_SrcVal(                     0)
    l_key(getLocalId(0) + (LOCAL_SIZE_LIMIT / 2)) = d_SrcKey((LOCAL_SIZE_LIMIT / 2))
    l_val(getLocalId(0) + (LOCAL_SIZE_LIMIT / 2)) = d_SrcVal((LOCAL_SIZE_LIMIT / 2))

    
    for(val size: UInt <- 2 unitl arrayLength){
        //Bitonic merge
        val ddd: UInt = dir ^ ( (get_local_id(0) & (size / 2)) != 0 )
        
        for(val stride <- size / 2  until stride > 0){
        
            localMem.barrier
            val pos: UInt = 2 * getLocalId(0) - (getLocalId(0) & (stride - 1))
            
            ComparatorLocal(
                l_key(pos +      0).p, l_val(pos +      0).p,
                l_key(pos + stride).p, l_val(pos + stride).p,
                ddd
            )
            stride >>= 1
          }
          size <<= 1
    }

    //ddd == dir for the last bitonic merge step
    {
        for(val stride: UInt <- arrayLength / 2 until 0){
        
            localMem.barrier
            val pos: UInt = 2 * getLocalId(0) - (getLocalId(0) & (stride - 1))
            ComparatorLocal(
                l_key(pos +      0).p, l_val(pos +      0).p,
                l_key(pos + stride).p, l_val(pos + stride).p,
                dir
            )
            stride >>= 1
        }
    }

    localMem.barrier
    
    d_DstKey(                     0) = l_key(getLocalId(0) +                      0)
    d_DstVal(                     0) = l_val(getLocalId(0) +                      0)
    d_DstKey((LOCAL_SIZE_LIMIT / 2)) = l_key(getLocalId(0) + (LOCAL_SIZE_LIMIT / 2))
    d_DstVal((LOCAL_SIZE_LIMIT / 2)) = l_val(getLocalId(0) + (LOCAL_SIZE_LIMIT / 2))
}

def GPU_bitonicMergeGlobal(d_DstKey: GlobalUIntP, d_DstVal: GlobalUIntP, d_SrcKey: GlobalUIntP, d_SrcVal: GlobalUIntP,
    arrayLength: UInt,
    size: UInt,
    stride: UInt,
    dir: UInt ) ={
    
    val global_comparatorI: UInt = getGlobalId(0)
    val comparatorI: UInt = global_comparatorI & (arrayLength / 2 - 1)

    val ddd: UInt = dir ^ ( (comparatorI & (size / 2)) != 0 );
    val pos: UInt = 2 * global_comparatorI - (global_comparatorI & (stride - 1));

    val keyA: UInt = d_SrcKey(pos +      0)
    val valA: UInt = d_SrcVal(pos +      0)
    val keyB: UInt = d_SrcKey(pos + stride)
    val valB: UInt = d_SrcVal(pos + stride)

    // ComparatorPrivate
    if( (keyA > keyB) == dir ){
        (keyA, keyB) = (keyB, keyA)
        (valA, valB) = (valB, valA)
    }

    d_DstKey(pos +      0) = keyA
    d_DstVal(pos +      0) = valA
    d_DstKey(pos + stride) = keyB
    d_DstVal(pos + stride) = valB
}

// @local Array[UInt]
// @global Array[UInt]
def GPU_bitonicMergeLocal(d_DstKey: GlobalUIntP, d_DstVal: GlobalUIntP, d_SrcKey: GlobalUIntP, d_SrcVal: GlobalUIntP,
    arrayLength: UInt,
    size: UInt,
    stride: UInt,
    dir: UInt ) ={
    
    val l_key= new Array[LocalUInt](LOCAL_SIZE_LIMIT)
    val l_val= new Array[LocalUInt](LOCAL_SIZE_LIMIT)

    d_SrcKey += getGroupId(0) * LOCAL_SIZE_LIMIT + getLocalId(0)
    d_SrcVal += getGroupId(0) * LOCAL_SIZE_LIMIT + getLocalId(0)
    d_DstKey += getGroupId(0) * LOCAL_SIZE_LIMIT + getLocalId(0)
    d_DstVal += getGroupId(0) * LOCAL_SIZE_LIMIT + getLocalId(0)
    l_key(getLocalId(0) +                      0) = d_SrcKey(                     0)
    l_val(getLocalId(0) +                      0) = d_SrcVal(                     0)
    l_key(getLocalId(0) + (LOCAL_SIZE_LIMIT / 2)) = d_SrcKey((LOCAL_SIZE_LIMIT / 2))
    l_val(getLocalId(0) + (LOCAL_SIZE_LIMIT / 2)) = d_SrcVal((LOCAL_SIZE_LIMIT / 2))

    val comparatorI: UInt = getGlobalId(0) & ((arrayLength / 2) - 1)
    val ddd: UInt = dir ^ ( (comparatorI & (size / 2)) != 0 )
    
    for(; stride > 0; stride >>= 1){
    
        localMem.barrier
        
        val pos: UInt = 2 * getLocalId(0) - (getLocalId(0) & (stride - 1))
        
        ComparatorLocal(
             l_key(pos +      0).p, l_val(pos +      0).p,
	     l_key(pos + stride).p, l_val(pos + stride).p,
             ddd
        )
    }

    localMem.barrier
    
    d_DstKey(                     0) = l_key(getLocalId(0) +                      0)
    d_DstVal(                     0) = l_val(getLocalId(0) +                      0)
    d_DstKey((LOCAL_SIZE_LIMIT / 2)) = l_key(getLocalId(0) + (LOCAL_SIZE_LIMIT / 2))
    d_DstVal((LOCAL_SIZE_LIMIT / 2)) = l_val(getLocalId(0) + (LOCAL_SIZE_LIMIT / 2))
    
  }

}
