package katas.game_of_life;

public final class PredefinedPatterns
{
    public static Generation block(final Bounds bounds)
    {
        final int center = bounds.center();
        return new Generation(bounds, new int[] { center - bounds.width - 1, center - bounds.width, center - 1, center });
    }

    public static Generation beehive(final Bounds bounds)
    {
        final int center = bounds.center();
        return new Generation(bounds, new int[] {
                center - bounds.width,
                center - bounds.width - 1,
                center + 1,
                center - 2,
                (center - 1) + bounds.width,
                center + bounds.width });
    }

    public static Generation blinker(final Bounds bounds)
    {
        final int center = bounds.center();
        final int left = center - 1;
        final int right = center + 1;
        return new Generation(bounds, new int[] { left, center, right });
    }

    public static Generation toad(final Bounds bounds)
    {
        final int row1 = bounds.center();
        final int row2 = row1 + bounds.width;
        return new Generation(bounds, new int[] { row1 - 1, row1, row1 + 1, row2 - 1, row2 - 2, row2 });
    }

    public static Generation beacon(final Bounds bounds)
    {
        final int row0 = bounds.center() - bounds.width;
        final int row1 = bounds.center();
        final int row2 = row1 + bounds.width;
        final int row3 = row2 + bounds.width;
        return new Generation(bounds, new int[] {
                row0 - 1, row0 - 2, row1 - 1, row1 - 2, row2, row2 + 1, row3, row3 + 1 });
    }

    public static Generation glider(final Bounds bounds)
    {
        final int row1 = bounds.center();
        final int row2 = row1 + bounds.width;
        final int row3 = row2 + bounds.width;
        return new Generation(bounds, new int[] {
                row1, row2, row3, row3 - 1, row2 - 2 });
    }

    public static Generation lwss(final Bounds bounds)
    {
        final int row0 = bounds.center() - bounds.width;
        final int row1 = bounds.center();
        final int row2 = row1 + bounds.width;
        final int row3 = row2 + bounds.width;
        return new Generation(bounds, new int[] {
                row0 - 1, row0 - 4, row1, row2, row3, row3 - 1, row3 - 2, row3 - 3, row2 - 4 });
    }

    /**
     * Calculates the same live cell pattern as used in the APL video:
     * http://www.youtube.com/watch?v=a9xAKttWgP4
     */
    public static Generation f_pentomino(final Bounds bounds)
    {
        final int row1 = bounds.center();
        final int row2 = row1 + bounds.width;
        final int row3 = row2 + bounds.width;
        return new Generation(bounds, new int[] {
                row1, row1 + 1, row2, row2 - 1, row3 });
    }

    public static Generation diehard(final Bounds bounds)
    {
        final int row1 = bounds.center();
        final int row2 = row1 + bounds.width;
        final int row3 = row2 + bounds.width;
        return new Generation(bounds, new int[] {
                row2 - 4, row2 - 3, row3 - 3, row3 + 1, row3 + 2, row3 + 3, row1 + 2 });
    }

    public static Generation acorn(final Bounds bounds)
    {
        final int row1 = bounds.center();
        final int row2 = row1 + bounds.width;
        final int row3 = row2 + bounds.width;
        return new Generation(bounds, new int[] {
                row1 - 3, row2 - 1, row3 - 3, row3 - 4, row3, row3 + 1, row3 + 2 });
    }
}
