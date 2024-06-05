import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class SharedCounter {
    private int counter = 0;    //shared counter
    private final Object syncObject = new Object();
    private final Lock lock = new ReentrantLock();  

    // using synchronized keyword to ensure that no race condition arises between incrementSynchronized and incrementWithLock
    public synchronized void incrementSynchronized() {   // incrementing using synchronized keyword for thread safety
        synchronized (syncObject) {
            counter++;
        }
    }

    public synchronized void incrementWithLock() {   // incrementing using lock method for thread safety
        lock.lock();
        try {
            counter++;
        } finally {         // exception handling
            lock.unlock();
        }
    }
    public int getCounter() {   // getter function
        return counter;
    }
}

class CounterUpdater extends Thread {
    private final SharedCounter sharedCounter;
    private final boolean usingSynch; // Indicates whether to use synchronized or ReentrantLock

    // Constructor taking a SharedCounter instance and a flag for synchronization method
    public CounterUpdater(SharedCounter sharedCounter, boolean usingSynch) {
        this.sharedCounter = sharedCounter;
        this.usingSynch = usingSynch;
    }

    // Run method to increment the shared counter 1000 times using the chosen synchronization method
    @Override
    public void run() {
        for (int i = 0; i < 1000; i++) {
            if (usingSynch) {
                sharedCounter.incrementSynchronized(); // Use synchronized
            } else {
                sharedCounter.incrementWithLock(); // Use ReentrantLock
            }
        }
    }
}

public class Main {
    public static void main(String[] args) {
        SharedCounter sharedCounter = new SharedCounter();

        // Create CounterUpdater 
        CounterUpdater updater1 = new CounterUpdater(sharedCounter, true); // Use synchronized
        CounterUpdater updater2 = new CounterUpdater(sharedCounter, false); // Use ReentrantLock
        //  CounterUpdater updater3 = new CounterUpdater(sharedCounter, false); // Use ReentrantLock

        updater1.start();
        updater2.start();
        // updater3.start();

        try {
            updater1.join();
            updater2.join();
            // updater3.join();
        } catch (InterruptedException e) {      // exception handling
            e.printStackTrace();
        }

        // Print the final value of the shared counter
        System.out.println("Final Counter Value: " + sharedCounter.getCounter());
    }
}


