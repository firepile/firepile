//Kernel code --


__kernel void VectorAdd(__global const float* a, __global const float* b, __global float* c, int iNumElements)
{
    // get index into global data array
    int iGID = get_global_id(0);

    // bound check (equivalent to the limit on a 'for' loop for standard/serial C code
    if (iGID >= iNumElements)
    {   
        return; 
    }
    
    // add the vector elements
    c[iGID] = a[iGID] + b[iGID];
}
//---------------------------------------End of Kernel Code----------------------------------------------------------------


//Scala code --

object VectorAdd {

  def main(args: Array[String]) = {
    val a = Array[Float](2.4f, 6.7f, 3.9f,23.53f)
    val b = Array[Float](14.12f, 23.55f, 34.6f, 12.1f)
    var c = Array[Float]

    c = vecAdd(a, b, 4)

    c.foreach(i => println(i))

  }
  
def vecAdd(a : Array[Float], b : Array[Float] , int iNumElements): Array[Float] {

...
...
val c = GPU_VecAdd(aa,bb,cc,iNumElements)
c
}

def GPU_vecAdd(a: GlobalFloatP, b: GlobalFloatP , c: GlobalFloatP , int iNumElements) = {

val iGID=getGlobalId(0)

if(iGID >= iNumElements)
return

c(iGID)=a(iGID)+b(iGID)

 }
 
 
}