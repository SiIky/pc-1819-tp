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

    private void handle_ingame ()
    {
        String line = "";

        /* TODO: tell the server whats happening on our side */
        line = arrows_to_string() + " "
            + eat_player_to_string()
            + eaten_to_string();

        System.out.println(line);
        this.st.out.println(line);
        this.st.out.flush();

        /* TODO: process what the server says happened on the other side */
        try {
            line = this.st.in.readLine();
            if (line == null) { /* socket was closed */
                this.st.screen = Screen.leave;
                return;
            }
        } catch (Exception e) {
            System.out.println("handle_ingame");
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

    public void run ()
    {
        try {
            while (this.st.screen != Screen.leave) {
                switch (this.st.screen) {
                    case ingame:  handle_ingame(); break;
                    case inqueue: handle_inqueue(); break;
                }
            }
        } catch (Exception e) {
            System.out.println("run");
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

    String eaten_to_string ()
    {
        String ret = " ";
        int len = this.st.eaten.size();

        if (len == 0)
            return "";

        ret = Integer.toString(this.st.eaten.get(0));

        for (int i = 1; i < len - 1; i++)
            ret = ret + ":" + Integer.toString(this.st.eaten.get(i));

        return ret;
    }

    String arrows_to_string ()
    {
        this.st.arrowsLock.lock();
        String ret = "";
        try {
            ret = ((this.st.arrows[0]) ? "U" : "_")
                + ((this.st.arrows[1]) ? "D" : "_")
                + ((this.st.arrows[2]) ? "L" : "_")
                + ((this.st.arrows[3]) ? "R" : "_");
        } catch (Exception e) {
            System.out.println("arrows_to_string");
        } finally {
            this.st.arrowsLock.unlock();
        }
        return ret;
    }

    String eat_player_to_string ()
    {
        float dist = distance(st.player.getX(), st.player.getY(), st.adversary.getX(), st.adversary.getY());
        return (dist < st.player.getRadius()/2 + st.adversary.getRadius()/2) ?
            " p":
            "";
    }
}
