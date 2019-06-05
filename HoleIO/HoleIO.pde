import java.net.Socket;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.InputStreamReader;

PFont font;
String time = "60";
int t;
int interval = 60;

State st = new State(); // 
BGThread bgt = new BGThread(st);

float a;

ArrayList<TextBox> textboxes = new ArrayList<TextBox>();

void setup()
{
    try {
        /* TODO: how do we know when the connection goes down? */
        st.sock = new Socket("localhost", 4242); //creates socket on port 4242
        st.in = new BufferedReader(new InputStreamReader(st.sock.getInputStream())); //init reads from server
        st.out = new PrintWriter(st.sock.getOutputStream(), true); //init writes to server
        font = createFont("Arial", 60);
        textFont(font);
    } catch (Exception e) {
        e.printStackTrace(); //case exception, leave program
        super.exit();
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
    background(100);
   
    t = interval - int(millis() / 1000);
    time = nf(t, 3);
    if (t == 0) {
      exit();
    }
    text(time, width - 120, 80);

    
    st.player.display(); 
    st.adversary.display();
    movePlayer();

    ArrayList<Integer> eaten = new ArrayList<Integer>(); //array list because we dont know how many were eaten

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
                    && !textboxes.get(1).Text.equals("")) //case id and pw are not empty
            {
                try {
                    String line = "login " + textboxes.get(0).Text + " " + textboxes.get(1).Text;
                    st.out.println(line); //sends login request info to server

                    line = st.in.readLine();
                    if (line.equals("ok")) {
                        st.player_name = textboxes.get(0).Text;
                        st.screen = Screen.inqueue;
                        bgt.start(); //new thread to receive text from server
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                    st.screen = Screen.leave;
                }
            } else {
                for (TextBox t : textboxes)
                    t.KEYPRESSED(key, keyCode); //if login is invalid continue.
            }
            break;
        case ingame:
            switch (keyCode) {
                case UP :
                case 'W' :
                    st.out.println("w");
                    st.arrows[0] = true;
                    break;
                case DOWN:
                case 'S':
                    st.out.println("s");
                    st.arrows[1] = true;
                    break;
                case 'A':
                case LEFT:
                    st.out.println("a");
                    st.arrows[2] = true;
                    break;
                case RIGHT:
                case 'D':
                    st.out.println("d");
                    st.arrows[3] = true;
                    break;
            }
            break;
        default: break;
    }
}

void keyReleased ()
{
    switch (keyCode) {
        case UP:
        case 'W':
            st.out.println("W");
            st.arrows[0] = false;
            break;
        case DOWN:
        case 'S':
            st.out.println("S");
            st.arrows[1] = false;
            break;
        case LEFT:
        case 'A':
            st.out.println("A");
            st.arrows[2] = false;
            break;
        case RIGHT:
        case 'D':
            st.out.println("D");
            st.arrows[3] = false;
            break;
    }
}

void exit ()
{
    st.screen = Screen.leave;
    try {
        bgt.interrupt();
        
        st.out.println("leave");
        st.in.close();
        st.out.close();
    } catch (Exception e) {
        e.printStackTrace();
    }
    System.exit(0);
}
