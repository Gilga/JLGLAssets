void synchronize()
{
    // Ensure that memory accesses to shared variables complete.
    memoryBarrierBuffer();
    memoryBarrierShared();
    groupMemoryBarrier();
    // Every thread in work group must reach this barrier before any other thread can continue.
    barrier();
}