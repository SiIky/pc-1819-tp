class Food
{
    int posx = 0;
    int posy = 0;
    int size = 0;
    boolean is_poison = false;
    boolean should_draw = false;

    Food () {}

    Food (int posx, int posy, int size, boolean is_poison) {
        this.posx = posx;
        this.posy = posy;
        this.size = size;
        this.is_poison = is_poison;
        this.should_draw = true;
    }

    // getters for position
    int getX()    { return this.posx; }
    int getY()    { return this.posy; }
    int getSize() { return this.size; }
    boolean is_poison() { return this.is_poison; }

    // setters for ball
    void setX(int posx) { this.posx = posx; }
    void setY(int posy) { this.posy = posy; }
    void setSize(int size) { this.size = size; }

    // picks a random location for object to be placed
    void pick_location(boolean is_poison) {
        this.posx = int(random(30, width - 30));
        this.posy = int(random(30, height - 30));
        this.is_poison = is_poison;
    }

    void display() {
        if (!should_draw)
            return;

        if (this.is_poison) {
            fill(255, 0, 0);
        } else {
            fill(0, 255, 0);
        }

        stroke(0, 0, 0);
        ellipse(this.posx, this.posy, this.size, this.size);
    }
}
