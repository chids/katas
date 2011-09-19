package katas.game_of_life.render;

import katas.game_of_life.Generation;

public final class ConsoleRenderer implements Renderer
{
    @Override
    public void render(final Generation generation)
    {
        print(generation);
    }

    public static void print(final Generation... views)
    {
        for(int cell = 0, size = views[0].bounds.width * views[0].bounds.height; cell < size; cell += views[0].bounds.width)
        {
            System.err.print(" | ");
            for(int i = 0; i < views.length; i++)
            {
                print(views[i], cell, cell + views[0].bounds.width);
                System.err.print("| ");
            }
            System.err.println();
        }
        System.err.println();
    }

    private static void print(final Generation view, final int from, final int to)
    {
        for(int i = from; i < to; i++)
        {
            System.err.print(view.asInt(i));
            System.err.print(' ');
        }
    }
}