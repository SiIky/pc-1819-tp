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
     * Arrows related methods
     */
    boolean[] arrows = new boolean[4]; // 0 -> up ; 1 -> down; 2 -> left; 3 -> right
    ReentrantLock arrowsLock = new ReentrantLock(); /* TODO: try the fairness param */
    void arrowsKeyReleased ()
    {
        st.arrowsLock.lock();
        try {
            switch (keyCode) {
                case UP:    st.arrows[0] = false; break;
                case DOWN:  st.arrows[1] = false; break;
                case LEFT:  st.arrows[2] = false; break;
                case RIGHT: st.arrows[3] = false; break;
            }
        } finally {
            st.arrowsLock.unlock();
        }
    }

    void arrowsKeyPressed ()
    {
        st.arrowsLock.lock();
        try {
            switch (keyCode) {
                case UP:    st.arrows[0] = true; break;
                case DOWN:  st.arrows[1] = true; break;
                case LEFT:  st.arrows[2] = true; break;
                case RIGHT: st.arrows[3] = true; break;
            }
        } finally {
            st.arrowsLock.unlock();
        }
    }
}
