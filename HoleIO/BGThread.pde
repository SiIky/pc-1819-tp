class BGThread extends Thread
{
    HoleIO.State st;

    public BGThread (HoleIO.State st)
    {
        this.st = st;
    }

    public void run ()
    {
        String line = "";
        try {
            do {
                line = st.in.readLine();
                System.out.println(line);
            } while (st.screen == Screen.ingame && !line.equals("leave_match"));
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            exit();
        }
    }
}
