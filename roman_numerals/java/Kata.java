import java.util.Arrays;

public final class Kata
{
    private static final String[] ONES = new String[] { "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX" };
    private static final String[] TENS = new String[] { "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC" };
    private static final String[] HUNDREDS = new String[] { "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM" };
    private static final Lookup[] LOOKUPS = new Lookup[] { new Lookup(ONES), new Lookup(TENS), new Lookup(HUNDREDS), new Lookup()
    {
        @Override
        String lookup(int arabic)
        {
            final char[] centuries = new char[arabic];
            Arrays.fill(centuries, 'M');
            return new String(centuries);
        }
    } };

    private static class Lookup
    {
        private final String[] values;

        Lookup(final String... values)
        {
            this.values = values;
        }

        String lookup(final int arabic)
        {
            return (arabic == 0) ? "" : this.values[arabic - 1];
        }
    }

    public String convert(final Integer number)
    {
        if(number == null || number < 1 || number > 3000)
        {
            throw new IllegalArgumentException();
        }
        else
        {
            final int[] numbers = asReversedIntArray(number);
            return toRoman(numbers).reverse().toString();
        }
    }

    private StringBuffer toRoman(final int[] numbers)
    {
        final StringBuffer result = new StringBuffer();
        for(int i = 0; i < numbers.length; i++)
        {
            result.append(LOOKUPS[i].lookup(numbers[i]));
        }
        return result;
    }

    private int[] asReversedIntArray(final Integer number)
    {
        final String reversedInput = new StringBuffer(number.toString()).reverse().toString();
        final int[] numbers = new int[reversedInput.length()];
        for(int i = 0; i < reversedInput.length(); i++)
        {
            numbers[i] = Integer.valueOf(reversedInput.substring(i, i + 1));
        }
        return numbers;
    }

    public static void main(String[] args)
    {
        final Kata kata = new Kata();
        for(final int number : new int[] { 1, 10, 100, 1000, 1234, 1990, 2008 })
        {
            System.err.println(number + "\t" + kata.convert(number));
        }
    }
}
