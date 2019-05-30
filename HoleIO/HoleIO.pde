import java.net.*;

/*
 * TODO: game state should be in its own class, for easier sharing
 *       between this thread and the networking thread.
 *       No Nestum BS pls though
 */

Ball player;

int number_of_consumables = 30;
Food[] consumables;
Socket con;
// 0 -> up ; 1 -> down; 2 -> left; 3 -> right
boolean[] arrows = new boolean[4];

/*
 * TODO: register/login & waiting screen. Maybe different `draw()`
 *       functions, 1 for each state. Maybe login & register can be
 *       the same screen: 2 fields (uname, passwd), 2 buttons (login,
 *       register)
 */
enum Screen {
    login,
    inqueue,
    ingame,
};
Screen screen = Screen.login; /* starts in the login screen */

void setup()
{
    try {
        /* TODO: how do we know when the connection goes down? */
        con = new Socket("localhost", 4242);
    } catch (Exception e) {
        exit();
    }

    size(1200, 700);
    //size(800, 600);

    player = new Ball(100, 100, true);
    consumables = new Food[number_of_consumables];

    /* TODO: this will be done in the server */
    for(int i = 0; i < number_of_consumables; i++) {
        boolean poison_or_not = random(0, 1) > 0.7; // 30% chance of being poison
        consumables[i] = new Food(poison_or_not);
    }

    frameRate(60);
}

void draw()
{
    background(100);
    player.display();
    movePlayer();

    for(int i = 0; i < number_of_consumables; i++) {
        consumables[i].display();

        float dist = distance(player.getX(), player.getY(), consumables[i].getX(), consumables[i].getY());
        if (dist < player.getRadius()/2 + consumables[i].getSize()/2) {
            player.eats(consumables[i]); /* we dont care if its poison or not, just eat that */
            boolean poison_or_not = random(0, 1) > 0.7;
            consumables[i].pick_location(poison_or_not);
        }
    }
}

// calculates euclidean distance
float distance(int p1x, int p1y, int p2x, int p2y) {
    float p = p2x - p1x;
    float q = p2y - p1y;

    return sqrt(p*p + q*q);
}

void movePlayer()
{
    if (arrows[0]) player.moveY(-1);
    if (arrows[1]) player.moveY(+1);
    if (arrows[2]) player.moveX(-1);
    if (arrows[3]) player.moveX(+1);
}

void keyPressed()
{
    if(keyCode == UP)    { arrows[0] = true; }
    if(keyCode == DOWN)  { arrows[1] = true; }
    if(keyCode == LEFT)  { arrows[2] = true; }
    if(keyCode == RIGHT) { arrows[3] = true; }
}

void keyReleased()
{
    if(keyCode == UP)    { arrows[0] = false; }
    if(keyCode == DOWN)  { arrows[1] = false; }
    if(keyCode == LEFT)  { arrows[2] = false; }
    if(keyCode == RIGHT) { arrows[3] = false; }
}
