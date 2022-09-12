/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Fall 2020                                 *
 **********************************************
 */
#include <stdio.h>
#include <stdlib.h>

__global__ void markFilterEdges_gpu(int * src, int * dst, int * matches, int * keepEdges, int numEdges) {
	/** YOUR CODE GOES BELOW **/
	

	int totalThreads = blockDim.x * gridDim.x;
	int tid = (blockDim.x * blockIdx.x) + threadIdx.x;

	if(totalThreads >= numEdges){

		if (tid < numEdges){

			if((matches[src[tid]] == -1) && (matches[dst[tid]] == -1)){

				keepEdges[tid] = 1;

			}else{

				keepEdges[tid] = 0;
			}
		}
	}else{

		for (int i = tid; i < numEdges; i += totalThreads){


				if((matches[src[i]] == -1) && (matches[dst[i]] == -1)){

					keepEdges[i] = 1;

				}else{

					keepEdges[i] = 0;
				}

		}	
	}



	/** YOUR CODE GOES ABOVE **/
}
