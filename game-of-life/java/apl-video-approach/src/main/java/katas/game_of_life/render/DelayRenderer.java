package katas.game_of_life.render;
import java.util.concurrent.TimeUnit;

import katas.game_of_life.Generation;

public final class DelayRenderer implements Renderer
{
    private final Renderer delegate;
    private final long delay;

    public DelayRenderer(final Renderer delegate, final long delay, final TimeUnit delayUnit)
    {
        this.delegate = delegate;
        this.delay = delayUnit.toMillis(delay);
    }

    @Override
    public void render(final Generation generation)
    {
        this.delegate.render(generation);
        try
        {
            Thread.sleep(this.delay);
        }
        catch(InterruptedException e)
        {
            throw new RuntimeException(e.getMessage(), e);
        }
    }
}