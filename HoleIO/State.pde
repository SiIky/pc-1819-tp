import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.concurrent.locks.ReentrantLock;

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

    /*
     * Eatten food methods
     */
    volatile ArrayList<Integer> eaten = new ArrayList<Integer>();
    volatile boolean done_eating = false;

    /*
     * Arrows related methods
     */
    boolean[] arrows = new boolean[4]; // 0 -> up ; 1 -> down; 2 -> left; 3 -> right
    ReentrantLock arrowsLock = new ReentrantLock(); /* TODO: try the fairness param */
    void arrowsKeyReleased ()
    {
        arrowsLock.lock();
        try {
            switch (keyCode) {
                case UP:    arrows[0] = false; break;
                case DOWN:  arrows[1] = false; break;
                case LEFT:  arrows[2] = false; break;
                case RIGHT: arrows[3] = false; break;
            }
        } finally {
            arrowsLock.unlock();
        }
    }

    void arrowsKeyPressed ()
    {
        arrowsLock.lock();
        try {
            switch (keyCode) {
                case UP:    arrows[0] = true; break;
                case DOWN:  arrows[1] = true; break;
                case LEFT:  arrows[2] = true; break;
                case RIGHT: arrows[3] = true; break;
            }
        } finally {
            arrowsLock.unlock();
        }
    }
}
