package aoc2016;

/**
 * Created by val on 05/12/17.
 */
public class Day23 {

    public enum InstrType {
        CPY, INC, DEC, JNZ, TGL
    }

    public interface IInstrArg{
        int resolveAsInt(int[] registers);
        int resolveAsReg();
    }

    public static class ConstantArg implements IInstrArg{
        public final int v;
        public ConstantArg(int v){
            this.v = v;
        }

        public int resolveAsInt(int[] registers) {
            return v;
        }
        public int resolveAsReg(){
            return -1;
        }
    }

    public static class RefArg implements IInstrArg{
        public final int reg;

        public RefArg(int reg){
            this.reg = reg;
        }

        public int resolveAsInt(int[] registers) {
            return registers[this.reg];
        }
        public int resolveAsReg(){
            return reg;
        }
    }

    public static class Instr {
        public InstrType instrType;
        public final IInstrArg arg1;
        public final IInstrArg arg2;

        public Instr(InstrType instrType, IInstrArg arg1, IInstrArg arg2){
            this.instrType = instrType;
            this.arg1 = arg1;
            this.arg2 = arg2;
        }
    }

    public static int cpy(int i, int[] registers, IInstrArg x, IInstrArg y){
        int reg = y.resolveAsReg();
        if(reg == -1){
            return i+1;
        } else {
            registers[reg] = x.resolveAsInt(registers);
            return i+1;
        }
    }

    public static int inc(int i, int[] registers, IInstrArg x){
        int reg = x.resolveAsReg();
        if(reg != -1){
            registers[reg] = registers[reg] + 1;
        }
        return i+1;
    }

    public static int dec(int i, int[] registers, IInstrArg x){
        int reg = x.resolveAsReg();
        if(reg != -1){
            registers[reg] = registers[reg] - 1;
        }
        return i+1;
    }

    public static int jnz(int i, int[] registers, IInstrArg x, IInstrArg y){
        int ix = x.resolveAsInt(registers);

        if(ix == 0){
            return i+1;
        } else {
            int iy = y.resolveAsInt(registers);
            return i + iy;
        }
    }

    public static InstrType toggleInstrType (InstrType instrType){
        switch (instrType){
            case INC:
                return InstrType.DEC;

            case DEC:
                return InstrType.INC;

            case TGL:
                return InstrType.INC;

            case JNZ:
                return InstrType.CPY;

            case CPY:
                return InstrType.JNZ;

            default:
                throw new IllegalArgumentException();
        }
    }

    public static int tgl(int i, int[] registers, Instr[] code, IInstrArg x){
        int ix = x.resolveAsInt(registers);
        int z = i + ix;
        if(z >= 0 && z < code.length){
            Instr toToggle = code[z];
            toToggle.instrType = toggleInstrType(toToggle.instrType);
        }
        return i+1;
    }

    public static int execInstr(int i, int[] registers, Instr[] code, Instr instr){
        switch (instr.instrType){
            case CPY:
                return cpy(i, registers, instr.arg1, instr.arg2);

            case INC:
                return inc(i, registers, instr.arg1);

            case DEC:
                return dec(i, registers, instr.arg1);

            case TGL:
                return tgl(i, registers, code, instr.arg1);

            case JNZ:
                return jnz(i, registers, instr.arg1, instr.arg2);

            default:
                throw new IllegalStateException();
        }
    }
}
