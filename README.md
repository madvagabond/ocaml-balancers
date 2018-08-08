#Balancer
A collection of load balancing algorithms implemented in ocaml.

The supported Algorithms are P2C, P2C_PKG, Round Robin, Consistent Hashing, and Consistent Hashing + Least loaded.


Also included are utilities for thread safe shared mutable state.
Easy compatability with service discovery by using React.


I'm honestly surprised there isn't a library for this already considering the way to scale ocaml is just to spawn a bunch of worker processes.

