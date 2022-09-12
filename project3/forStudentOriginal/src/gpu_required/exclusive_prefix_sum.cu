/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Fall 2020                                 *
 **********************************************
 */
#include <stdio.h>
#include <stdlib.h>

__global__ void exclusive_prefix_sum_gpu(int * oldSum, int * newSum, int distance, int numElements) {
	/** YOUR CODE GOES BELOW **/

	
	int totalThreads = blockDim.x * gridDim.x;
	int tid = (blockDim.x * blockIdx.x) + threadIdx.x;

	if (totalThreads >= numElements){

		if(distance == 0){
			if (tid == 0){
				newSum[tid] = 0;
			}else if (tid < numElements){
				newSum[tid] = oldSum[tid - 1];
			}
		}else{
			if((tid >= distance) && (tid < numElements)){
					
				newSum[tid] = oldSum[tid] + oldSum[tid - distance];

			}else if (tid < distance){
				newSum[tid] = oldSum[tid];
			}
		}
	}else{
		for(int i = tid; i < numElements; i += totalThreads){

			if(distance == 0){
				if (i == 0){
					newSum[i] = 0;
				}else{
					newSum[i] = oldSum[i - 1];
				}
			}else{
				if(i >= distance){
				
					newSum[i] = oldSum[i] + oldSum[i - distance];

				}else if (i < distance){
					newSum[i] = oldSum[i];
				}
			}

		}
	}


	/** YOUR CODE GOES ABOVE **/
}
