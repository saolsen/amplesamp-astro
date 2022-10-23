import { createClient } from "@supabase/supabase-js";
import type { AstroCookies } from "astro/dist/core/cookies";

export default async function getUserConnection(cookies: AstroCookies) {
  const cookie = cookies.get("access-token");
  if (cookie && cookie.value !== "") {
    const supabase = createClient(
      import.meta.env.PUBLIC_SUPABASE_URL,
      import.meta.env.PUBLIC_SUPABASE_KEY
    );
    const {
      data: { user },
    } = await supabase.auth.getUser(cookie.value);
    if (user && user.role == "authenticated") {
      return { supabase, user };
    }
  }

  return null;
}
