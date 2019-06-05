import java.net.Socket;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.InputStreamReader;

class Register
{
    public static void main (String[] args) throws Exception
    {
        if (args.length != 2) {
            System.out.println("Usage: USERNAME PASSWORD");
            System.exit(1);
        }

        Socket sock;

        sock = new Socket("localhost", 4242);

        BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()));
        PrintWriter out = new PrintWriter(sock.getOutputStream());

        String line = "register " + args[0] + " " + args[1];
        out.println(line);
        out.flush();

        String res = in.readLine();
        System.out.println(res);
    }
}
