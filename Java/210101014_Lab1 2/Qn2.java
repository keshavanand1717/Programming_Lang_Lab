import java.util.Scanner;
public class Qn2 {

    private static int counter = 0; // shared counter variable intialised to 0

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in); // taking input from the user
        System.out.println("Enter the number of times to increment the counter:");
        int n = sc.nextInt();   // increment times
        System.out.println("Enter the number of times to decrement the counter:");
        int m = sc.nextInt();   // decrement times

        Thread increment = new Thread(new IncrementTask(n));    // threads intialised
        Thread decrement = new Thread(new DecrementTask(m));
        increment.start();      // functions in the runnable class started concurrently
        decrement.start();

        try {           // waiting for the threads to finish
            increment.join();
            decrement.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("Final Counter Value: " + counter);  // output printed
    }
    static class IncrementTask implements Runnable {    // runnable class for incrementing
        private int n;
        public IncrementTask(int n){
            this.n = n;
        }
        @Override
        public void run() {
            for (int i = 0; i < n; i++) {       // running for loop the number of times (n) specified by the user
                synchronized (Qn2.class) {      // synchronising to avoid race condition.
                    counter++;
                }
            }
        }
    }
    static class DecrementTask implements Runnable {    // runnable class for decrementing
        private int m;
        public DecrementTask(int m){
            this.m = m;
        }
        @Override
        public void run() {
            for (int i = 0; i < m; i++) {       // running for loop the number of times (m) specified by the user
                synchronized (Qn2.class) {      // synchronising to avoid race condition.
                    counter--;
                }
            }
        }
    }
}
