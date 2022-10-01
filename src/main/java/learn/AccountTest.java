package learn;

public class AccountTest {
    public static void main(String[] args) {


        System.out.println("Testing Account Class");

        //create an object
        Account ac1 = new Account();    //default constructor
//        System.out.println("Balance of ac1 : " + ac1.balance);  //Error
        System.out.println("Balance in ac1 : " + ac1.getBalance());
        ac1.deposite(2500.0);
        ac1.withdraw(120.0);
        ac1.deposite(200.0);

        Account ac2 = new Account("Jim", 5000.0);   //parameterized constructor
        ac2.deposite(6000.0);
        ac2.withdraw(200.0);
        ac2.deposite(-1200.0);
        ac2.withdraw(40000.0);

//        ac1.balance = 100000.0; //Error
        ac1.deposite(100000.0);
//        System.out.println("Balance in ac1 : " + ac1.balance);  //Error
        System.out.println("Balance in ac1 : " + ac1.getBalance());

//        ac1.ownerName = "John"; //Error
        ac1.setOwnerName("John");
        ac1.deposite(100.0);

    }
}
