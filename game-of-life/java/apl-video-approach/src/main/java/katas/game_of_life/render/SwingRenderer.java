package katas.game_of_life.render;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Panel;
import java.awt.image.BufferedImage;

import javax.swing.JFrame;

import katas.game_of_life.Generation;

public final class SwingRenderer implements Renderer
{
    private static final String TITLE_TEMPLATE = "Game of Life - Generation: %d";
    private final Panel panel;
    private final int magnifier;
    private final int rows;
    private Graphics2D graphics;
    protected final int width;
    protected final int height;
    protected BufferedImage image;

    public SwingRenderer(final int rows, final int columns, final int magnifier)
    {
        this.rows = rows;
        this.magnifier = magnifier;
        this.width = rows * magnifier;
        this.height = columns * magnifier;
        this.image = new BufferedImage(this.width, this.height, BufferedImage.TYPE_BYTE_BINARY);
        this.graphics = this.image.createGraphics();
        this.panel = new Panel()
        {
            private static final long serialVersionUID = 1L;
            private final JFrame frame = new JFrame();
            private int generation = 0;

            {
                this.frame.getContentPane().add(this);
                this.frame.setSize(SwingRenderer.this.width, SwingRenderer.this.height);
                this.frame.setVisible(true);
            }

            @Override
            public void paint(final Graphics g)
            {
                g.drawImage(SwingRenderer.this.image, 0, 0, null);
                this.frame.setTitle(String.format(TITLE_TEMPLATE, this.generation++));
            }
        };
    }

    @Override
    public void render(final Generation generation)
    {
        this.graphics.clearRect(0, 0, this.width, this.height);
        for(final int cell : generation.alive())
        {
            final int row = cell / this.rows;
            final int column = cell % this.rows;
            this.graphics.drawRect(column * this.magnifier, row * this.magnifier, this.magnifier, this.magnifier);
        }
        this.panel.repaint();
    }
}