package learn;

public class Account {

    //attributes
    private String ownerName;
    private Double balance;

//    //default constructor
    Account(){
        this.ownerName = "Unknown";
        this.balance = 0.0;
    }

    //parameterized constructor
    Account(String name, Double amount){
        this.ownerName = name;
        this.balance = amount;
    }

    //getters
    Double getBalance(){
        return this.balance;
    }

    String getName(){
        //verify ID
        return  this.ownerName;
    }

    //setters
    void setOwnerName(String name){
        this.ownerName = name;
    }

    //functions
    void deposite(Double amount){
        System.out.println("Trying to deposit " + amount + " to " + this.ownerName + " account.");
        if (amount <= 0){
            System.out.println("Transaction not supported. Please enter correct amount.");
        }else {
            this.balance += amount;
        }
        System.out.println("Updated Balance : " + this.balance);
    }

    void withdraw(Double amount){
        System.out.println("Trying to withdraw " + amount + " from " + this.ownerName + " account.");
        if (this.balance < amount){
            System.out.println("Transaction not supported. Not enough balance in the account.");
        }else{
            this.balance -= amount;
        }

        System.out.println("Updated Balance : " + this.balance);
    }

}
