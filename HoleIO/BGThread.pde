class BGThread extends Thread
{
    HoleIO.State st;

    boolean should_run = true;

    public BGThread (HoleIO.State st)
    {
        this.st = st;
    }

    private void handle_inqueue ()
    {
        try {
            String line = st.in.readLine();
            if (line == null) { /* socket was closed */
                this.st.screen = Screen.leave;
                return;
            }

            System.out.println(line);

            String[] words = line.split(" ");

            if (!words[0].equals("enter_match"))
                return;

            this.st.player    = ball_from_str(words[1], true);
            this.st.adversary = ball_from_str(words[2], false);
            this.st.adversary_name = words[3];

            for (int i = 0; i < this.st.number_of_consumables; i++) {
                String[] parms = words[4 + i].split(":");
                int fidx = Integer.parseInt(parms[0]);
                this.st.consumables[fidx].update_from_parms(parms);
            }

            st.screen = Screen.ingame;
        } catch (Exception e) {
            this.should_run = false;
            return;
        }
    }

    private void handle_ingame ()
    {
        try {
            String line = this.st.in.readLine();
            if (line == null) /* socket was closed */
                return;

            System.out.println(line);

            if (line.equals("leave_match")) {
                this.st.screen = Screen.inqueue;
                return;
            }

            String words[] = line.split(" ");

            this.st.player.update_from_str(words[0]);
            this.st.adversary.update_from_str(words[1]);

            int nfood = words.length - 2;
            for (int i = 0; i < nfood; i++) {
                String[] parms = words[2 + i].split(":");
                int fidx = Integer.parseInt(parms[0]);
                this.st.consumables[fidx].update_from_parms(parms);
            }
        } catch (Exception e) {
            this.should_run = false;
            return;
        }
    }

    public void run ()
    {
        try {
            while (this.should_run) {
                switch (this.st.screen) {
                    case ingame:  handle_ingame(); break;
                    case inqueue: handle_inqueue(); break;
                    default: return;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            this.st.screen = Screen.leave;
            return;
        }
    }

    Ball ball_from_str (String str, boolean is_player_1)
    {
        String[] parms = str.split(":");
        int x = Integer.parseInt(parms[0]);
        int y = Integer.parseInt(parms[1]);
        return new Ball(x, y, is_player_1);
    }
}
