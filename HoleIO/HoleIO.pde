import java.net.Socket;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.InputStreamReader;

State st = new State();
BGThread bgt;

float a;

ArrayList<TextBox> textboxes = new ArrayList<TextBox>();

void setup()
{
    try {
        /* TODO: how do we know when the connection goes down? */
        st.sock = new Socket("localhost", 4242);
        st.in = new BufferedReader(new InputStreamReader(st.sock.getInputStream()));
        st.out = new PrintWriter(st.sock.getOutputStream());
    } catch (Exception e) {
        exit();
    }

    size(1200, 700);
    //size(800, 600);

    // USERNAME TextBox
    // CONFIGURED USING THE GLOBAL VARS
    TextBox userTB = new TextBox(160, 103, 200, 35);

    // PASSWORD TextBox
    // CONFIGURED USING THE CLASS CONSTRACTOR
    TextBox passTB = new TextBox(160, 153, 200, 35);
    passTB.BorderWeight = 3;
    passTB.BorderEnable = true;

    textboxes.add(userTB);
    textboxes.add(passTB);

    frameRate(60);

    stroke(255);
    a = height/2;
}

void draw()
{
    switch (st.screen) {
        case ingame:  draw_ingame(); break;
        case inqueue: draw_inqueue(); break;
        case login:   draw_login(); break;
        case leave:   exit(); break;
    }
}

void draw_login ()
{
    background(40, 160, 40);

    // Labels
    fill(250, 250, 250);
    text("LOGIN FORM", (width - textWidth("LOGIN FORM")) / 2, 60);
    textSize(15);
    text("Press Enter to Login", (width - textWidth("Press Enter to Login")) / 2, 80);
    textSize(24);
    text("Username: ", 20, 130);
    text("Password: ", 20, 180);

    // Draw the textboxes
    for (TextBox t : textboxes)
        t.DRAW();
}

void draw_inqueue ()
{
    background(51);
    line(0, a, width, a);
    a -= 0.5;
    if (a < 0)
        a = height;
}

void mousePressed() {
    for (TextBox t : textboxes)
        t.PRESSED(mouseX, mouseY);
}

void draw_ingame ()
{
    /* TODO: communicate with the server */
    background(100);
    st.player.display();
    st.adversary.display();
    movePlayer();

    ArrayList<Integer> eaten = new ArrayList<Integer>();

    for (int i = 0; i < st.number_of_consumables; i++) {
        if (st.consumables[i].should_draw) {
            st.consumables[i].display();

            float dist = distance(st.player.getX(), st.player.getY(), st.consumables[i].getX(), st.consumables[i].getY());
            if (dist < st.player.getRadius()/2 + st.consumables[i].getSize()/2) {
                st.player.eats(st.consumables[i]); /* we dont care if its poison or not, just eat that */
                st.consumables[i].should_draw = false;
                eaten.add(i);
            }
        }
    }

    this.st.eaten = eaten;
    this.st.done_eating = true;
}

// calculates euclidean distance
float distance (int p1x, int p1y, int p2x, int p2y) {
    float p = p2x - p1x;
    float q = p2y - p1y;
    return sqrt(p*p + q*q);
}

void movePlayer ()
{
    st.arrowsLock.lock();
    try {
        if (st.arrows[0]) st.player.moveY(-1);
        if (st.arrows[1]) st.player.moveY(+1);
        if (st.arrows[2]) st.player.moveX(-1);
        if (st.arrows[3]) st.player.moveX(+1);
    } catch (Exception e) {
        System.out.println("movePlayer");
        st.screen = Screen.leave;
    } finally {
        st.arrowsLock.unlock();
    }
}

void keyPressed ()
{
    switch (st.screen) {
        case login:
            if (keyCode == ENTER
                    && !textboxes.get(0).Text.equals("")
                    && !textboxes.get(1).Text.equals(""))
            {
                try {
                    String line = "login " + textboxes.get(0).Text + " " + textboxes.get(1).Text;
                    st.out.println(line);
                    st.out.flush();

                    line = st.in.readLine();
                    if (line.equals("ok")) {
                        st.player_name = textboxes.get(0).Text;
                        st.screen = Screen.inqueue;

                        bgt = new BGThread(st);
                        bgt.start();
                    }
                } catch (Exception e) {
                    System.out.println("keyPressed");
                    st.screen = Screen.leave;
                }
            } else {
                for (TextBox t : textboxes)
                    t.KEYPRESSED(key, keyCode);
            }
            break;
        case ingame:
            st.arrowsKeyPressed();
            break;
        default: break;
    }
}

void keyReleased ()
{
    st.arrowsKeyReleased();
}
