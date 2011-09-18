package katas.game_of_life;
import java.util.BitSet;

public final class Generation
{
    public final int height;
    public final int width;
    private final BitSet world;

    public Generation(final int width, final int height)
    {
        this.width = width;
        this.height = height;
        this.world = new BitSet(width * height);
    }
    
    public Generation(final int width, final int height, final int[] alive)
    {
        this(width, height);
        for(final int cell : alive)
        {
            this.world.set(cell);
        }
    }
    
    private Generation(final BitSet world, final int width, final int height)
    {
        this.width = width;
        this.height = height;
        this.world = world;
    }

    public int asInt(final int i)
    {
        return this.world.get(i) ? 1 : 0;
    }

    public int[] alive()
    {
        final int[] tmp = new int[size()];
        int copied = 0;
        for(int i = this.world.nextSetBit(0); i >= 0; i = this.world.nextSetBit(i + 1))
        {
            tmp[copied++] = i;
        }
        final int[] result = new int[copied];
        System.arraycopy(tmp, 0, result, 0, copied);
        return result;
    }

    public int size()
    {
        return this.width * this.height;
    }

    public Generation and(final BitSet other)
    {
        final BitSet result = (BitSet)this.world.clone();
        result.and(other);
        return new Generation(result, this.width, this.height);
    }

    public Generation or(final BitSet other)
    {
        final BitSet result = (BitSet)this.world.clone();
        result.or(other);
        return new Generation(result, this.width, this.height);
    }
}