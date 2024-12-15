public class zd1 {

    public static int[] removeElement(final int[] array, final int element) {
        int resultLength = 0;

        for (final int value : array) {
            if (value != element) {
                resultLength++;
            }
        }

        final int result[] = new int[resultLength];
        int currentIndex = 0;

        for (final int value : array) {
            if (value != element) {
                result[currentIndex++] = value;
            }
        }

        return result;
    }

    public static void printArray(final int[] array) {
        System.out.print("[");

        if (array.length != 0) {
            System.out.print(array[0]);

            for (int i = 1; i < array.length; i++) {
                System.out.printf(", %d", array[i]);
            }
        }

        System.out.println("]");
    }

    public static void main(final String[] args) {
        final int[] testCase1 = new int[] { 1, 2, 2, 2, 3, 1, 4 };
        printArray(removeElement(testCase1, 2));

        final int[] testCase2 = new int[] { 1, 2, 3, 4, 1, 2, 3, 4 };
        printArray(removeElement(testCase2, 3));

        final int[] testCase3 = new int[] {};
        printArray(removeElement(testCase3, 5));

        final int[] testCase4 = new int[] { 1, 2, 3 };
        printArray(removeElement(testCase4, 4));

        final int[] testCase5 = new int[] { 2, 2, 2, 2, 2 };
        printArray(removeElement(testCase5, 2));

        final int[] testCase6 = new int[] { 5 };
        printArray(removeElement(testCase6, 5));

        final int[] testCase7 = new int[] { 5 };
        printArray(removeElement(testCase7, 2));
    }
}
