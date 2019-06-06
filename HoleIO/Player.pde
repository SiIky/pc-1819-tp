class Player
{
    int posx;  // position on the x axis
    int posy;  // position on the y axis

    int radius;  // value increases when ball "eats" an object

    boolean is_player_1; // player 1 is controlled by the user; player 2 is the adversary (controlled by someone else)

    static final int min_radius = 15;  // min. value possible for the radius
    static final int max_radius = 200;

    Player (int posx, int posy, boolean is_player_1) //ball constructor
    {
        this.posx = posx;
        this.posy = posy;
        this.radius = min_radius;
        this.is_player_1 = is_player_1;
    }

    void update_from_str (String str)
    {
        String[] parms = str.split(":");
        this.posx = Integer.parseInt(parms[0]); //player doesnt need index
        this.posy = Integer.parseInt(parms[1]);
        this.radius = Integer.parseInt(parms[2]);
    }

    // getters for ball
    int getX() { return this.posx; }
    int getY() { return this.posy; }
    int getRadius() { return this.radius; }

    // setters for ball
    void setX(int posx) { this.posx = posx; }
    void setY(int posy) { this.posy = posy; }
    void setRadius(int radius) { this.radius = radius; }

    int calc_speed ()
    {
        return floor(150 / this.radius) + 5;
    }

    // moves player on the x axis
    void moveX (int x)
    {
        this.posx = constrain(this.posx + x * calc_speed(), floor(this.radius / 2), width - floor(this.radius / 2));
    }

    // moves player on the y axis
    void moveY (int y)
    {
        this.posy = constrain(this.posy + y * calc_speed(), floor(this.radius / 2), height - floor(this.radius / 2));
    }

    // eats a poison consumable
    private void eats_poison (Food poison)
    {
        this.radius = constrain(this.radius - poison.getSize(), min_radius, max_radius);
    }

    // eats an edible consumable
    private void eats_food (Food consumable)
    {
        this.radius = constrain(this.radius + consumable.getSize(), min_radius, max_radius);
    }

    void eats (Food consumable)
    {
        if (consumable.is_poison())
            this.eats_poison(consumable);
        else
            this.eats_food(consumable);
    }

    // prints object to screen
    void display ()
    {
        if (this.is_player_1)
            stroke(0, 0, 255);
        else
            stroke(255, 0, 0);
        fill(0, 0, 0);
        ellipse(this.posx, this.posy, this.radius, this.radius);
    }

    void eat_player (Player adv)
    {
        this.radius = constrain(this.radius + int(adv.getRadius()/4), min_radius, max_radius);
    }
}
