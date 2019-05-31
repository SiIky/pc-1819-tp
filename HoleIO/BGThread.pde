class BGThread extends Thread
{
    HoleIO.State st;

    public BGThread (HoleIO.State st)
    {
        this.st = st;
    }

    public void run ()
    {
        while (true) {
            try {
                Thread.sleep(1000);
                System.out.println("wut");
            } catch (Exception e) {
                System.out.println("shat");
            }
        }
    }
}
