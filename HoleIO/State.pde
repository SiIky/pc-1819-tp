import java.net.Socket;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.InputStreamReader;

class State
{
    String player_name;
    String adversary_name;

    volatile boolean ingame = false;

    Ball player;
    Ball adversary;
    int number_of_consumables = 30;
    Food[] consumables;
    Socket sock;
    BufferedReader in;
    PrintWriter out;
    // 0 -> up ; 1 -> down; 2 -> left; 3 -> right
    boolean[] arrows = new boolean[4];
    Screen screen = Screen.login; /* starts in the login screen */
}
