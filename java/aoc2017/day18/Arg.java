package aoc2017.day18;

public interface Arg {
    long resolveValue(int[] state);
    int resolveRegister();
}
