/*
 * 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 * 
 * What is the sum of the digits of the number 2^1000?
 */

// All the heavy lifting is done in the LongNumber class.
EulerTimer {LongNumber(2).pow(1000).n.sum}
