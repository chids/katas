package katas.game_of_life;

import java.util.Arrays;
import java.util.BitSet;

import katas.game_of_life.render.Renderer;

/**
 * <ol>
 * <li>The Game of Life: http://en.wikipedia.org/wiki/Conway's_Game_of_Life</li>
 * <li>This is modelled after the APL solution shown here: http://www.youtube.com/watch?v=a9xAKttWgP4</li>
 * <li>The Coding Dojo Kata: http://codingdojo.org/cgi-bin/wiki.pl?KataGameOfLife</li>
 * </ol>
 * 
 * The rules:
 * 1. Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
 * 2. Any live cell with more than three live neighbours dies, as if by overcrowding.
 * 3. Any live cell with two or three live neighbours lives on to the next generation.
 * 4. Any dead cell with exactly three live neighbours becomes a live cell.
 */
public class GameOfLife
{
    private final Generation generation;

    public GameOfLife(final Generation generation)
    {
        this.generation = generation;
    }

    public GameOfLife next()
    {
        // Create all rotations for the current generation
        final Generation[] rotations = rotations(this.generation);
        // Merge all rotations and count each cells total occurrence
        // to get the total neighbour count for each cell
        final int[] sum = summarizeNeighbours(rotations);
        // Only keep cells that have 3 or 4 neighbours
        final BitSet[] kept = killAnythingBut3and4(sum);
        // Keep all 3's but only keep cells with 4 neighbours that
        // where alive in the original generation
        return new GameOfLife(this.generation.and(kept[1]).or(kept[0]));
    }

    private static BitSet[] killAnythingBut3and4(final int[] sums)
    {
        final BitSet[] result = new BitSet[] { new BitSet(sums.length), new BitSet(sums.length) };
        for(int i = 0; i < sums.length; i++)
        {
            final int family = (sums[i] == 3 || sums[i] == 4) ? sums[i] - 3 : -1;
            if(family >= 0)
            {
                result[family].set(i, true);
            }
        }
        return result;
    }

    private static int[] summarizeNeighbours(final Generation... rotations)
    {
        final int[] result = new int[rotations[0].bounds.size()];
        Arrays.fill(result, 0);
        for(final Generation rotation : rotations)
        {
            for(int cell : rotation.alive())
            {
                result[cell]++;
            }
        }
        return result;
    }

    private static Generation[] rotations(final Generation generation)
    {
        return new Generation[] {
                rotate(generation, -(generation.bounds.width - 1)), rotate(generation, -generation.bounds.width),
                rotate(generation, -(generation.bounds.width + 1)),
                rotate(generation, -1), generation, rotate(generation, 1),
                rotate(generation, generation.bounds.width - 1), rotate(generation, generation.bounds.width), rotate(generation, generation.bounds.width + 1) };
    }

    private static Generation rotate(final Generation generation, final int direction)
    {
        final int[] alive = generation.alive();
        final int[] result = new int[alive.length];
        for(int i = 0; i < alive.length; i++)
        {
            final int offset = alive[i] + direction;
            final int flip;
            // Edge wrap logic
            if(offset < 0)
            {
                flip = generation.bounds.size() + offset;
            }
            else if(offset >= generation.bounds.size())
            {
                flip = offset - generation.bounds.size();
            }
            else
            {
                flip = offset;
            }
            result[i] = flip;
        }
        return generation.copy(result);
    }

    public void renderTo(final Renderer renderer)
    {
        renderer.render(this.generation);
    }
}