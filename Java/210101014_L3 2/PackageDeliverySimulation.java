import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

// Class representing a DeliveryMan with attributes mentioned in the problem
class DeliveryMan implements Runnable {
    private int deliveryManId;
    private int packageId;
    private int deliveryTime;

    // Constructor to initialize delivery attributes
    public DeliveryMan(int deliveryManId, int packageId, int deliveryTime) {
        this.deliveryManId = deliveryManId;
        this.packageId = packageId;
        this.deliveryTime = deliveryTime;
    }

    // Run method representing the delivery process
    @Override
    public void run() {
        try {
            // Package out for delivery
            synchronized (DeliveryMan.class) {
                printMessage("[Package " + packageId + "] out for delivery with deliveryman id " + deliveryManId +
                    ". It will take " + deliveryTime + " seconds.");
            }
          

            // Simulate package delivery
            TimeUnit.SECONDS.sleep(deliveryTime);

            // Package delivered
            synchronized (DeliveryMan.class) {
                printMessage("[Package " + packageId + "] delivered by deliveryman id " + deliveryManId + ".");
            }
            // Deliveryman returns to the courier office
            synchronized (DeliveryMan.class) {
                printMessage("Deliveryman " + deliveryManId + " returned to the courier office.");
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    // Utility method to print messages with timestamps
    private void printMessage(String message) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        String timestamp = dateFormat.format(new Date());
        System.out.println("[" + timestamp + "] " + message);
    }
}

public class PackageDeliverySimulation {
    public static void main(String[] args) {
        // Scanner object for user input
        Scanner scanner = new Scanner(System.in);
        System.out.print("Enter the number of packages: ");
        int numPackages = scanner.nextInt();
        System.out.print("Enter the number of deliverymen: ");
        int numDeliveryMen = scanner.nextInt();

        // Closing the scanner
        scanner.close();

        // Creating a thread pool with the specified number of deliverymen
        ExecutorService executorService = Executors.newFixedThreadPool(numDeliveryMen);

        // Create and submit delivery tasks to the thread pool
        int deliveryTime = (int) (Math.random() * 2) + 1;  
        for (int i = 1; i <= numPackages; i++) {
            deliveryTime += (int) (Math.random() * 2) + 1;  // Random delivery times for the packages
            DeliveryMan deliveryMan = new DeliveryMan(i%numDeliveryMen+101,i, deliveryTime);
            executorService.submit(deliveryMan);
        }

        // Shutdown the thread pool after all tasks are submitted
        executorService.shutdown();
    }
}
