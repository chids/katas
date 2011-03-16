public final class Kata
{
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
            result.append(ArabicToRoman.TRANSLATIONS[i].translate(numbers[i]));
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
