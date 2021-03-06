class BGThread extends Thread //background thread
{
    HoleIO.State st;

    public BGThread (HoleIO.State st)
    {
        this.st = st;
    }

    private void handle_inqueue ()
    {
        try {
            String line = st.in.readLine(); //reads the line receive from server
            if (line == null) { /* socket was closed */
                this.st.screen = Screen.leave; //if there's nothing exit.
                return;
            }

            System.out.println(line); 

            String[] words = line.split(" "); //insert in array elements by space

            if (words[0].equals("updated_scores")) {
                for (int i = 0; i < 5; i++)
                    this.st.top_scores[i] = "";

                for (int i = 1; i < words.length; i++) {
                    String[] SN = words[i].split(":");
                    this.st.top_scores[5 - i] = "(" + SN[1] + ", " + SN[0] + ")";
                }
            } else if (words[0].equals("enter_match")) {
                this.st.player    = ball_from_str(words[1], true); //receives word from server and converts it to player ball object
                this.st.adversary = ball_from_str(words[2], false); // "" adversary ball obj
                this.st.adversary_name = words[3]; // to draw adv name

                for (int i = 0; i < this.st.number_of_consumables; i++) {
                    String[] parms = words[4 + i].split(":"); // parse food info from server into array
                    int fidx = Integer.parseInt(parms[0]); // parse food index into array to know which obj was eaten
                    this.st.consumables[fidx] = food_from_parms(parms);
                }

                st.game_start_time = millis();
                st.screen = Screen.ingame; //update to ingame
            }
        } catch (Exception e) {
            this.st.screen = Screen.leave;
            return;
        }
    }

    private void handle_ingame ()
    {
        try {
            String line = this.st.in.readLine();
            if (line == null) { /* socket was closed */
                this.st.screen = Screen.leave;
                return;
            }

            System.out.println(line);

            if (line.equals("leave_match")) {
                this.st.screen = Screen.end_game;
                return;
            }

            String words[] = line.split(" ");

            this.st.player.update_from_str(words[0]); // when ingame receive info from server to update size, position etc.
            this.st.adversary.update_from_str(words[1]); //same  but for adv

            int nfood = words.length - 2; //number of food positions
            for (int i = 0; i < nfood; i++) {
                String[] parms = words[2 + i].split(":"); //parse into array food arguments
                int fidx = Integer.parseInt(parms[0]); //same as before
                this.st.consumables[fidx] = food_from_parms(parms);
            }
        } catch (Exception e) {
            this.st.screen = Screen.leave;
            return;
        }
    }

    public void run ()
    {
        try {
            while (this.st.screen != Screen.leave) { //while there's no exception
                switch (this.st.screen) { //evaluate game state
                    case ingame:   handle_ingame(); break;
                    case inqueue:  handle_inqueue(); break;
                    case end_game: handle_inqueue(); break;
                    default: return;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            this.st.screen = Screen.leave;
            return;
        }
    }

    Player ball_from_str (String str, boolean is_player_1) // construct ball from information received from server.
    {
        String[] parms = str.split(":");
        int x = Integer.parseInt(parms[0]);
        int y = Integer.parseInt(parms[1]);
        return new Player(x, y, is_player_1);
    }

    Food food_from_parms (String[] parms)
    {
        return new Food(
                Integer.parseInt(parms[1]),
                Integer.parseInt(parms[2]),
                Integer.parseInt(parms[3]),
                parms[4].equals("true"));
    }
}
