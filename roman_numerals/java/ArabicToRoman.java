import java.util.Arrays;

class ArabicToRoman
{
    private static final String[] ONES = new String[] { "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX" };
    private static final String[] TENS = new String[] { "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC" };
    private static final String[] HUNDREDS = new String[] { "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM" };
    static final ArabicToRoman[] TRANSLATIONS = new ArabicToRoman[] { new ArabicToRoman(ONES), new ArabicToRoman(TENS), new ArabicToRoman(HUNDREDS), new ThousandsTranslation() };

    private final String[] values;

    ArabicToRoman(final String... values)
    {
        this.values = values;
    }

    String translate(final int arabic)
    {
        return (arabic == 0) ? "" : this.values[arabic - 1];
    }

    static final class ThousandsTranslation extends ArabicToRoman
    {
        @Override
        String translate(int arabic)
        {
            final char[] centuries = new char[arabic];
            Arrays.fill(centuries, 'M');
            return new String(centuries);
        }
    }
}