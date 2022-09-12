/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Fall 2020                                 *
 **********************************************
 */
#include <stdio.h>
#include <stdlib.h>

__global__ void check_handshaking_gpu(int * strongNeighbor, int * matches, int numNodes) {
	/** YOUR CODE GOES BELOW **/
	
		int totalThreads = blockDim.x * gridDim.x;
		int tid = (blockDim.x * blockIdx.x) + threadIdx.x;

		if (totalThreads >= numNodes){
			
			if(tid < numNodes){

				if(strongNeighbor[tid] != -1){

					if(strongNeighbor[strongNeighbor[tid]] == tid){

						matches[tid] = strongNeighbor[tid];

					}else{

						matches[tid] = -1;

					}
					

				}
					
			}	

		}else{

			for(int i = tid; i < numNodes; i += totalThreads){
				
				
				if(strongNeighbor[i] != -1){

					if(strongNeighbor[strongNeighbor[i]] == i){

						matches[i] = strongNeighbor[i];

					}else{

						matches[i] = -1;

					}
					

				}

			}

		}



	/** YOUR CODE GOES ABOVE **/
}
