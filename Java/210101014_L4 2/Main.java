import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.Scanner;

class FooBar {
    private int n;  // Number of times to repeat the sequence
    private final Lock locker = new ReentrantLock();  // Lock for synchronization
    private final Condition condition1 = locker.newCondition();  // Condition for 'foo' thread
    private final Condition condition2 = locker.newCondition();  // Condition for 'bar' thread
    private boolean turnFoo = true;  // Flag to indicate whose turn it is

    public FooBar(int n) {
        this.n = n;
    }

    public void foo() throws InterruptedException {
        for (int i = 0; i < n; i++) {
            locker.lock();  // Acquire the lock
            try {
                while (!turnFoo) {  // Wait until it's 'foo' thread's turn
                    condition1.await();
                }
                System.out.print("foo");
                turnFoo = false;  // Switch to 'bar' thread's turn
                condition2.signal();  // Signal 'bar' thread to run
            } finally {
                locker.unlock();  // Release the lock
            }
        }
    }

    public void bar() throws InterruptedException {
        for (int i = 0; i < n; i++) {
            locker.lock();  // Acquire the lock
            try {
                while (turnFoo) {  // Wait until it's 'bar' thread's turn
                    condition2.await();
                }
                System.out.print("bar");
                turnFoo = true;  // Switch to 'foo' thread's turn
                condition1.signal();  // Signal 'foo' thread to run
            } finally {
                locker.unlock();  // Release the lock
            }
        }
    }
}

public class Main {
    public static void main(String[] args) {
        // Scanner object for user input
        Scanner scanner = new Scanner(System.in);
        System.out.print("Enter the number of times to repeat the sequence or enter 0 to select a random number of repeatitions: "); // specify the number of times to repeat the sequence
        int n = scanner.nextInt();
        if(n==0){
            n = (int) (Math.random() * 100) + 1; // Value of n will be a random number between 1-100
        }
        else if(n<0){
            while(n<0){
                System.out.print("The value of n cannot be negative.\nPlease enter a valid number of repetitions or enter 0 to select a random number of repeatitions:");
                n = scanner.nextInt();
            }
            if(n==0){
                n = (int) (Math.random() * 100) + 1;
            }
        }

        FooBar fooBar = new FooBar(n); // an object of the FooBar class intialised 

        // Thread 1 intialised which calls the foo function
        Thread threadFoo = new Thread(() -> {
            try {
                fooBar.foo();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        });

        // Thread 2 intialised which calls the bar function
        Thread threadBar = new Thread(() -> {
            try {
                fooBar.bar();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        });

        //Both the threads are started 
        threadFoo.start();
        threadBar.start();

        try {
            threadFoo.join();
            threadBar.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
