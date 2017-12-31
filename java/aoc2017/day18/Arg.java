package aoc2017.day18;

public interface Arg {
    long resolveValue(long[] state);
    int resolveRegister();
}
