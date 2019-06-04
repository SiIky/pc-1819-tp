import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

class State
{
    volatile Screen screen = Screen.login; /* starts in the login screen */

    Socket sock;
    BufferedReader in;
    PrintWriter out;

    String player_name;
    String adversary_name;

    Ball player;
    Ball adversary;

    final int number_of_consumables = 30;
    Food[] consumables = new Food[number_of_consumables];

    State ()
    {
        for (int i = 0; i < this.number_of_consumables; i++)
            consumables[i] = new Food();
    }

    /*
     * Eatten food methods
     */
    volatile ArrayList<Integer> eaten = new ArrayList<Integer>();
    volatile boolean done_eating = false;

    volatile boolean[] arrows = new boolean[4]; // 0 -> up ; 1 -> down; 2 -> left; 3 -> right
}
