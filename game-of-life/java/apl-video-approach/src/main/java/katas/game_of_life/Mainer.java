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
        final int width = Integer.parseInt(args[0]);
        final int height = Integer.parseInt(args[1]);
        final int delay = Integer.parseInt(args[2]);
        final boolean useSwing = args.length == 4;
        final Renderer renderer = createRenderer(width, height, delay, useSwing);
        final Bounds bounds = new Bounds(width, height);
        final Generation generation = PredefinedPatterns.f_pentomino(bounds);
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
            System.err.println(Mainer.class.getName() + " <width> <height> <ms-delay-between-each-generation> <use-swing>");
            System.exit(-1);
        }
    }
}
