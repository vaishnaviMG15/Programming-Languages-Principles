/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Fall 2020                                 *
 **********************************************
 */
#include <stdio.h>
#include <stdlib.h>

#ifndef gpu_headers_h
#define gpu_headers_h

/**
 * Updates matches based on strongNeighbor
 * @param strongNeighbor The strongest neighbor for every segment, or -1 for empty segments.
 * @param matches The matches (so far). Has been initialized with -1 values for each unmatched node.
 * @param numNodes The size of the strongNeighbor and res arrays.
 */
__global__ void check_handshaking_gpu(int * strongNeighbor, int * matches, int numNodes);

/**
 * Marks whether to keep or filter each edge: 1 to keep, 0 to filter.
 * @param src The source array for the edge list
 * @param dst The destination array for the edge list
 * @param matches The matches we've found so far (with -1 for unmatched nodes)
 * @param keepEdges The output of this GPU kernel function
 * @param numEdges The size of the src, dst, and keepEdges arrays.
 */
__global__ void markFilterEdges_gpu(int * src, int * dst, int * matches, int * keepEdges, int numEdges);

/**
 * Performs one step of an exclusive prefix sum. This version is NOT segmented.
 * To implement exclusivity, when distance == 0 it should copy the value from distance of 1.
 * @param oldSum The prefix sum (so far) from the previous step
 * @param newSum The output of this GPU kernel function
 * @param distance The distance between elements being added together in this step, or 0 to shift right
 * @param numElements The size of each array
 */
__global__ void exclusive_prefix_sum_gpu(int * oldSum, int * newSum, int distance, int numElements);

/**
 * Repacks the edge list (i.e. the source, destination, and weight arrays), thereby filtering out some edges
 * @param newSrc The new source array produced by this GPU kernel function
 * @param oldSrc The old source array
 * @param newDst The new destination array produced by this GPU kernel function
 * @param oldDst The old destination array
 * @param newWeight The new weight array produced by this GPU kernel function
 * @param oldWeight The old weight array
 * @param edgeMap List of new indices for the old edges
 * @param numEdges The size of the oldSrc, oldDst, oldWeight, and edgeMap arrays.
 */
__global__ void packGraph_gpu(int * newSrc, int * oldSrc, int * newDst, int * oldDst, int * newWeight, int * oldWeight, int * edgeMap, int numEdges);

#endif