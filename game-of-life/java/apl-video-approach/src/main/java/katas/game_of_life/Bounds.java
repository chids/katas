package katas.game_of_life;

public final class Bounds
{
    public final int height;
    public final int width;

    public Bounds(final int width, final int height)
    {
        this.width = width;
        this.height = height;
    }

    public int size()
    {
        return this.width * this.height;
    }

    public int center()
    {
        return (this.width / 2) + (this.width * (this.height / 2));
    }
}
