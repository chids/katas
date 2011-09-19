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

    public int upperLeft()
    {
        return -lowerLeft();
    }

    public int upperRight()
    {
        return -lowerRight();
    }

    public int lowerLeft()
    {
        return below() - 1;
    }

    public int lowerRight()
    {
        return below() + 1;
    }

    public int below()
    {
        return this.width;
    }

    public int above()
    {
        return -below();
    }

    @SuppressWarnings("static-method")
    public int left()
    {
        return -1;
    }

    @SuppressWarnings("static-method")
    public int right()
    {
        return 1;
    }
}