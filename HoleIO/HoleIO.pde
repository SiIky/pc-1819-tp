import java.net.Socket;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.InputStreamReader;

/*
 * TODO: game state should be in its own class, for easier sharing
 *       between this thread and the networking thread.
 *       No Nestum BS pls though
 */

State st = new State();
BGThread bgt;

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
}

void draw()
{
    switch (st.screen) {
        case login: draw_login(); break;
        case inqueue: draw_inqueue(); break;
        case ingame: draw_ingame(); break;
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
    try {
        background(100);
        String line = st.in.readLine();
        if (line == null)
            exit();
        String[] words = line.split(" ");

        if (words[0].equals("enter_match")) {
            st.player = player_from_str(words[1], true);
            st.adversary = player_from_str(words[2], false);

            for (int i = 0; i < st.number_of_consumables; i++)
                st.consumables[i] = consumable_from_str(words[3 + i]);

            st.screen = Screen.ingame;
            bgt = new BGThread(st);
            bgt.start();
        }
    } catch (Exception e) {
        exit();
    }
}

void mousePressed() {
    for (TextBox t : textboxes) {
        t.PRESSED(mouseX, mouseY);
    }
}

void draw_ingame ()
{
    /* TODO: communicate with the server */
    background(100);
    st.player.display();
    st.adversary.display();
    movePlayer();

    for(int i = 0; i < st.number_of_consumables; i++) {
        st.consumables[i].display();

        float dist = distance(st.player.getX(), st.player.getY(), st.consumables[i].getX(), st.consumables[i].getY());
        if (dist < st.player.getRadius()/2 + st.consumables[i].getSize()/2) {
            st.player.eats(st.consumables[i]); /* we dont care if its poison or not, just eat that */
            boolean poison_or_not = random(0, 1) > 0.7;
            st.consumables[i].pick_location(poison_or_not);
        }
    }
}

// calculates euclidean distance
float distance (int p1x, int p1y, int p2x, int p2y) {
    float p = p2x - p1x;
    float q = p2y - p1y;
    return sqrt(p*p + q*q);
}

void movePlayer ()
{
    if (st.arrows[0]) st.player.moveY(-1);
    if (st.arrows[1]) st.player.moveY(+1);
    if (st.arrows[2]) st.player.moveX(-1);
    if (st.arrows[3]) st.player.moveX(+1);
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
                    String line = "login:" + textboxes.get(0).Text + "\t" + textboxes.get(1).Text;
                    st.out.println(line);
                    st.out.flush();

                    line = st.in.readLine();
                    if (line.equals("ok")) {
                        st.player_name = textboxes.get(0).Text;
                        st.screen = Screen.inqueue;
                    }
                } catch (Exception e) {}
            } else {
                for (TextBox t : textboxes)
                    t.KEYPRESSED(key, keyCode);
            }
            break;
        case inqueue: break;
        case ingame:
                      if(keyCode == UP)    st.arrows[0] = true;
                      if(keyCode == DOWN)  st.arrows[1] = true;
                      if(keyCode == LEFT)  st.arrows[2] = true;
                      if(keyCode == RIGHT) st.arrows[3] = true;
                      break;
    }
}

void keyReleased ()
{
    if(keyCode == UP)    { st.arrows[0] = false; }
    if(keyCode == DOWN)  { st.arrows[1] = false; }
    if(keyCode == LEFT)  { st.arrows[2] = false; }
    if(keyCode == RIGHT) { st.arrows[3] = false; }
}

void exit ()
{
    try {
        st.sock.close();
    } catch (Exception e) {
    } finally {
        super.exit();
    }
}

Ball player_from_str (String str, boolean is_player_1)
{
    String[] parms = str.split(":");

    int x = Integer.parseInt(parms[0]);
    int y = Integer.parseInt(parms[1]);

    return new Ball(x, y, is_player_1);
}

Food consumable_from_str (String str)
{
    String[] parms = str.split(":");

    int x = Integer.parseInt(parms[0]);
    int y = Integer.parseInt(parms[1]);
    int s = Integer.parseInt(parms[2]);
    boolean is_poison = parms[3].equals("true");

    return new Food(x, y, s, is_poison);
}
