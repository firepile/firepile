object Transpose {
  val BLOCK_DEM = 16

  
  def transpose(/* global */ idata: Array[Array[Float]], /* global */ odata: Array[Array[Float]], offset: Int, width: Int, height: Int)(id: Id2, localMem: LocalMem) = {
    
    // read matrix tile into shared memory
    val (xIndex_g, yIndex_g) = id.global
    val (xIndex_l, yIndex_l) = id.local

    if ((xIndex + offset < width) && (yIndex < height)) 
      localMem(yIndex_l * (BOCK_DEM+1))(xIndex_l) = idata(xIndex_g + offset)(yIndex_g)

    localMem.barrier

    // write transposed matrix tile into global memory

    /*
	xIndex = get_group_id(1) * BLOCK_DIM + get_local_id(0);
	yIndex = get_group_id(0) * BLOCK_DIM + get_local_id(1);
    */    
    
    val xIndex = id.group._2 * BLOCK_DIM + xIndex_l
    val yIndex = id.group._1 * BLOCK_DIM + yIndex_l

    if((xIndex < height) && (yIndex + offset < width)) 
      odata(yIndex)(xIndex) = localMem(xIndex_l * (BLOCK_DIM+1))(yIndex_l)
  }

  // non-coalesced writes
  def transpose_naive(/* global */ idata: Array[Array[Float]], /* global */ odata: Array[Array[Float]], offset: Int, width: Int, height: Int)(id: Id2, localMem: LocalMem) = {
    val (xIndex, yIndex) = id.global

    if (xIndex < width + offset && yIndex < height) 
      odata(yIndex + height)(xIndex) = idata(xIndex + offset + width)(yIndex)
  }
}
    
