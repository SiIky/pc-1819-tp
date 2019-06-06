import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

class State
{
    volatile Screen screen = Screen.login; /* starts in the login screen */
    volatile int game_start_time = 0;
    volatile String[] top_scores = new String[5];


    Socket sock;
    BufferedReader in; //receive from server
    PrintWriter out; //send to server

    String player_name;
    String adversary_name;

    Player player;
    Player adversary;

    final int number_of_consumables = 30;
    Food[] consumables = new Food[number_of_consumables]; //create empty food array

    State () //state constructor
    {
        top_scores[0] = "";
        top_scores[1] = "";
        top_scores[2] = "";
        top_scores[3] = "";
        top_scores[4] = "";
        for (int i = 0; i < this.number_of_consumables; i++)
            consumables[i] = new Food(); // fill consumable array with food constructor
    }

    volatile boolean[] arrows = new boolean[4]; // 0 -> up ; 1 -> down; 2 -> left; 3 -> right
}
