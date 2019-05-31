class BGThread extends Thread
{
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
