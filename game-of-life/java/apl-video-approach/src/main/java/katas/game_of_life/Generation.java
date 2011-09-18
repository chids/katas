package katas.game_of_life;

import java.util.BitSet;

public final class Generation
{
    public final Bounds bounds;
    private final BitSet world;

    public Generation(final Bounds bounds)
    {
        this.bounds = bounds;
        this.world = new BitSet(bounds.size());
    }

    public Generation(final Bounds bounds, final int[] alive)
    {
        this(bounds);
        for(final int cell : alive)
        {
            this.world.set(cell);
        }
    }

    private Generation(final BitSet world, final Bounds bounds)
    {
        this.bounds = bounds;
        this.world = world;
    }

    public int asInt(final int i)
    {
        return this.world.get(i) ? 1 : 0;
    }

    public int[] alive()
    {
        final int[] tmp = new int[this.bounds.size()];
        int copied = 0;
        for(int i = this.world.nextSetBit(0); i >= 0; i = this.world.nextSetBit(i + 1))
        {
            tmp[copied++] = i;
        }
        final int[] result = new int[copied];
        System.arraycopy(tmp, 0, result, 0, copied);
        return result;
    }

    public Generation and(final BitSet other)
    {
        final BitSet result = (BitSet)this.world.clone();
        result.and(other);
        return new Generation(result, this.bounds);
    }

    public Generation or(final BitSet other)
    {
        final BitSet result = (BitSet)this.world.clone();
        result.or(other);
        return new Generation(result, this.bounds);
    }

    public Generation copy(final int[] alive)
    {
        return new Generation(this.bounds, alive);
    }
}