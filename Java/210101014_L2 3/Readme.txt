Lab 2 Submission
Anand Keshav - 210101014
Qn brief desc. - Shared counter variable to be incremented using 2 synchronization methods

I have followed all the pointwise instructions as mentioned in the mail with the problem statement verbatim. I have written comments 
to explain the different parts of the program. 

To compile the program use command "javac Main.java" in the terminal
To run the program use command "java Main" in the terminal


Some critical points in the program are - 
- Counter is intialised to 0.
- Since, we had to implement two instances of CounterUpdater, one for each synchronization technique, I have used a boolean 
  (usingSynch) as a flag to specify which technique is used in the run function. So, I have 2 parameters sharedCounter and usingSynch
  in the constructor of CounterUpdater.
- Usingsynch is TRUE means we are using synchronized keyword to increment and lock is used in the other case. This is handled using
  if-else statement in the run function.
- I have used synchronized keyword to ensure that no race condition arises between incrementSynchronized and incrementWithLock,
  so we don't face any race conditions and get the desired counter value finally which is 2000.

In case of any discrepancies please write to - anand.keshav@iitg.ac.in