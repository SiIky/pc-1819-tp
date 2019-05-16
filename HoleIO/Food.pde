class Food
{
  int posx;
  int posy;
  int size;
  boolean is_poison;
 
  Food(int posx, int posy, int size, boolean is_poison) {
    this.posx = posx;
    this.posy = posy;
    this.size = size;
    this.is_poison = is_poison;
  }
 
  // getters for position
  int getX()    { return this.posx; }
  int getY()    { return this.posy; }
  int getSize() { return this.size; }
  boolean get_is_poison() { return this.is_poison; }
 
  // setters for ball
  void setX(int posx) { this.posx = posx; }
  void setY(int posy) { this.posy = posy; }
  void setSize(int size) { this.size = size; }
 
  // picks a random location for object to be placed
  void pick_location() {
    this.posx = int(random(width));
    this.posy = int(random(height));
  }
 
 void display() {
  if (this.is_poison) {
    fill(255, 0, 0); 
  } else {
    fill(0, 255, 0);
  }
  
  stroke(0, 0, 0);
  ellipse(this.posx, this.posy, this.size, this.size);
 }
}
