class BGThread extends Thread
{
    HoleIO.State st;

    public BGThread (HoleIO.State st)
    {
        this.st = st;
    }

    private void handle_inqueue () throws Exception
    {
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

        for (int i = 0; i < this.st.number_of_consumables; i++)
            this.st.consumables[i] = food_from_str(words[4 + i]);

        st.screen = Screen.ingame;
    }

    private void handle_ingame () throws Exception
    {
        while (true) {
            /* TODO: tell the server whats happening on our side */
            /* TODO: process what the server says happened on the other side */
            String line = st.in.readLine();
            if (line == null) { /* socket was closed */
                this.st.screen = Screen.leave;
                return;
            }

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
        }
    }

    public void run ()
    {
        try {
            while (this.st.screen != Screen.leave) {
                switch (st.screen) {
                    case ingame:  handle_ingame(); break;
                    case inqueue: handle_inqueue(); break;
                }
            }
        } catch (Exception e) {
            this.st.screen = Screen.leave;
        }
    }

    Ball ball_from_str (String str, boolean is_player_1)
    {
        String[] parms = str.split(":");
        int x = Integer.parseInt(parms[0]);
        int y = Integer.parseInt(parms[1]);
        return new Ball(x, y, is_player_1);
    }

    Food food_from_str (String str)
    {
        String[] parms = str.split(":");
        int x = Integer.parseInt(parms[0]);
        int y = Integer.parseInt(parms[1]);
        int s = Integer.parseInt(parms[2]);
        boolean is_poison = parms[3].equals("true");
        return new Food(x, y, s, is_poison);
    }
}
