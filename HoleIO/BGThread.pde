class BGThread extends Thread
{
    volatile HoleIO.State st;

    public BGThread (HoleIO.State st)
    {
        this.st = st;
    }

    public void run ()
    {
        try {
            String line = "";
            while (st.screen == Screen.ingame) {
                line = st.in.readLine();
                System.out.println(line);
            }
            st.screen = Screen.inqueue;
        } catch (Exception e) {}
    }
}
