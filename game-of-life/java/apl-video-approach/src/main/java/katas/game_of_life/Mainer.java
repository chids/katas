package katas.game_of_life;

import java.util.concurrent.TimeUnit;

import katas.game_of_life.render.ConsoleRenderer;
import katas.game_of_life.render.DelayRenderer;
import katas.game_of_life.render.Renderer;
import katas.game_of_life.render.SwingRenderer;

public class Mainer
{
    public static void main(String[] args)
    {
        assertArguments(args);
        final int width = Integer.parseInt(args[0]); // 35;
        final int height = Integer.parseInt(args[1]); // 15;
        final int delay = Integer.parseInt(args[2]); // 100
        final boolean useSwing = args.length == 4;
        final Renderer renderer = createRenderer(width, height, delay, useSwing);
        final Bounds bounds = new Bounds(width, height);
        final Generation generation = new Generation(bounds, aplVideoCells(bounds));
        for(GameOfLife game = new GameOfLife(generation);; game = game.next())
        {
            game.renderTo(renderer);
        }
    }

    private static Renderer createRenderer(final int width, final int height, final int delay, final boolean useSwing)
    {
        final int magnifier = 20;
        return new DelayRenderer(
                useSwing ? new SwingRenderer(width, height, magnifier) : new ConsoleRenderer(),
                delay,
                TimeUnit.MILLISECONDS);
    }

    private static void assertArguments(final String[] args)
    {
        if(args.length < 3)
        {
            usage();
            System.exit(-1);
        }
    }

    private static void usage()
    {
        System.err.println(Mainer.class.getName() + " <width> <height> <ms-delay-between-each-generation> <use-swing>");
    }

    /**
     * Calculates the same live cell pattern as used in the APL video:
     * http://www.youtube.com/watch?v=a9xAKttWgP4
     */
    public static int[] aplVideoCells(final Bounds bounds)
    {
        final int center = bounds.center();
        return new int[] { center - 1, center - bounds.width + 1, center - bounds.width, center, center + bounds.width };
    }

    public static int[] block(final Bounds bounds)
    {
        final int center = bounds.center();
        return new int[] { center - bounds.width - 1, center - bounds.width, center - 1, center };
    }

    public static int[] beehive(final Bounds bounds)
    {
        final int center = bounds.center();
        return new int[] { center - bounds.width, center - bounds.width - 1, center + 1, center - 2, (center - 1) + bounds.width, center + bounds.width };
    }

    public static int[] blinker(final Bounds bounds)
    {
        final int center = bounds.center();
        final int left = center - 1;
        final int right = center + 1;
        return new int[] { left, center, right };
    }

    public static int[] toad(final Bounds bounds)
    {
        final int row1 = bounds.center();
        final int row2 = row1 + bounds.width;
        return new int[] { row1 - 1, row1, row1 + 1, row2 - 1, row2 - 2, row2 };
    }
}
