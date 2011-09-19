package katas.game_of_life;

public final class Rotation
{
    public static Generation[] rotations(final Generation generation)
    {
        final Bounds bounds = generation.bounds;
        return new Generation[] {
                rotate(generation, bounds.upperLeft()), rotate(generation, bounds.above()), rotate(generation, bounds.upperRight()),
                rotate(generation, bounds.left()), generation, rotate(generation, bounds.right()),
                rotate(generation, bounds.lowerLeft()), rotate(generation, bounds.below()), rotate(generation, bounds.lowerRight()) };
    }

    private static Generation rotate(final Generation generation, final int direction)
    {
        final Bounds bounds = generation.bounds;
        final int[] alive = generation.alive();
        final int[] result = new int[alive.length];
        for(int i = 0; i < alive.length; i++)
        {
            final int offset = alive[i] + direction;
            final int flip;
            // Edge wrap logic
            if(offset < 0)
            {
                flip = bounds.size() + offset;
            }
            else if(offset >= bounds.size())
            {
                flip = offset - bounds.size();
            }
            else
            {
                flip = offset;
            }
            result[i] = flip;
        }
        return new Generation(generation.bounds, result);
    }
}