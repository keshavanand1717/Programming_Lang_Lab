import java.util.Scanner;
public class MatrixMultiply {

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.println("Enter the matrix size ");
        int n = sc.nextInt();   // matrix size input (assuming square matrix)
        int[][] matrixA = new int[n][n];
        int[][] matrixB = new int[n][n];
        System.out.println("Enter the elements of the first matrix"); // input the matrix elements
        for(int i=0;i<n;i++){
            for(int j=0;j<n;j++){
                matrixA[i][j] = sc.nextInt();
            }
        }
        System.out.println("Enter the elements of the second matrix");  // input the matrix elements
        for(int i=0;i<n;i++){
            for(int j=0;j<n;j++){
                matrixB[i][j] = sc.nextInt();
            }
        }
        int[][] result = new int[n][n]; // result matrix
        int numThreads = n; // number of threads is equal to the number of rows
        Thread[] threads = new Thread[numThreads]; // threads declared
        for (int i = 0; i < numThreads; i++) {
            int rowNum = i;
            threads[i] = new Thread(new MatrixMultiplier(matrixA, matrixB, result, rowNum));    // threads initialised
            threads[i].start();     // threads started
        }

        try {
            for (Thread thread : threads) { // waiting for all threads to finish
                thread.join();
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("Result Matrix:");
        for(int i=0;i<n;i++)
        {
            for(int j=0;j<n;j++){
                 System.out.print(result[i][j]+ " ");
            }
            System.out.println();
        }
    }

    static class MatrixMultiplier implements Runnable { // Runnable class for matrix multiplication
        private int[][] matrixA;
        private int[][] matrixB;
        private int[][] result;
        private int rowNum;

        public MatrixMultiplier(int[][] matrixA, int[][] matrixB, int[][] result, int rowNum) { //constructor
            this.matrixA = matrixA;
            this.matrixB = matrixB;
            this.result = result;
            this.rowNum = rowNum;
        }

        @Override
        public void run() {     // each thread runs this funstion
            int i = rowNum;
            for (int j = 0; j < matrixB[0].length; j++) {
                for (int k = 0; k < matrixA[0].length; k++) {
                    result[i][j] += matrixA[i][k] * matrixB[k][j];  // result matrix updated
                }
            }
        }
    }

    
}
