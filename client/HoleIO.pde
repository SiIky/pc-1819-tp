Ball player;
Food consumable;

// 0 -> up ; 1 -> down; 2 -> left; 3 -> right
boolean[] arrows = new boolean[4]; 

void setup()
{
  size(800, 600);
  
  player = new Ball(100, 100);
  consumable = new Food(int(random(width + 15)), int(random(height - 15)), 30, false);
  //consumable = new Food(500, 200, 30, false);
  
  frameRate(60);
}

void draw()
{
  background(100);
  player.display();
  movePlayer();
  
  consumable.display();
  
  if (consumable.get_is_poison()) {
    float dist = distance(player.getX(), player.getY(), consumable.getX(), consumable.getY());
    if (dist < player.getRadius()/2 + consumable.getSize()/2) {
      print("eats venom -- ");
      player.eats_poison(consumable);
      
    }
  } else {
    float dist = distance(player.getX(), player.getY(), consumable.getX(), consumable.getY());
    if (dist < player.getRadius()/2 + consumable.getSize()/2) {
      player.eats_food(consumable);
      consumable.pick_location();
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
 if (arrows[0]) {
   player.moveY(-1);
 } if (arrows[1]) {
   player.moveY(+1);
 } if (arrows[2]) {
   player.moveX(-1);
 } if (arrows[3]) {
   player.moveX(+1);
 }
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
