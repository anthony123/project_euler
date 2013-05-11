/*
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143 ?
 */

// This problem is solved in our EulerLong class, in a relatively straightforward way: it computes the requested
// number's prime factors and extracts the largest.
// There might very well be a more clever approach, but this one has the advantage of being generic.

import EulerLong._

EulerTimer {600851475143l.primeFactors.max}

