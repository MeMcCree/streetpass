//Streetpass gamemode - made by BtC/BlaxorTheCat https://steamcommunity.com/id/BlaxorTheCat/ and Envy https://steamcommunity.com/id/Envy-Chan/
//maps using this gamemode use the sp_ prefix

const VERSION = "1.7-stam";
const SWAP_SOUND = "coach/coach_look_here.wav";
PrecacheSound(SWAP_SOUND);

Convars.SetValue("tf_passtime_ball_reset_time", 99999);
Convars.SetValue("tf_passtime_powerball_threshold", 10000);
Convars.SetValue("tf_passtime_powerball_passpoints", 1);
Convars.SetValue("tf_passtime_powerball_decayamount", 99999);
Convars.SetValue("tf_passtime_powerball_decayamount", 99999);
Convars.SetValue("tf_passtime_overtime_idle_sec", 99999);

/*  1.6.22 change list:

    fixed blitz activating if the round is over
    *made half-zatoichi not be able to kill others (UNTESTED)
    *fix steal overlay not properly clearing sometimes (UNTESTED)
*/

// StreetPASS convars
::streetpassConvars <- {
    ["sp_medic_replicates_democharge"] = {type = "int", value = 0, desc = "Allows the medic to mimic demomans shield charge while holding the ball", def = 0},
    ["sp_medic_replicates_caber"] = {type = "int", value = 1, desc = "Allows the medic to mimic demomans caber jumps while holding the ball", def = 1},
    ["sp_medic_replicates_blast_jump"] = {type = "int", value = 1, desc = "Allows the medic to mimic blast jumps while holding the ball", def = 1},
    ["sp_demoman_minchargepercentage"] = {type = "float", value = 33.0, desc = "The % that the demomans shield will recharge to after a charge (0-100)", def = 33.0},
    ["sp_demoman_caber_recharge_time"] = {type = "float", value = 1, desc = "The time it takes for caber to recharge, -1 = dont recharge", def = 1},
    ["sp_pyro_primary_charge_rate"] = {type = "float", value = 0.8, desc = "How fast can pyro fire dragons fury", def = 0.8},
    ["sp_pyro_df_splash_radius"] = {type = "float", value = 38.5, desc = "Splash Radius on the dragons fury", def = 38.5},
    ["sp_pyro_detonator_knockback_mult"] = {type = "float", value = 1.6, desc = "Self Knockback multiplier on the detonator", def = 1.6},
    ["sp_pyro_detonator_splash_radius"] = {type = "float", value = 56.0, desc = "Detonator Ball Splash radius", def = 56.0},
    ["sp_infinite_clip"] = {type = "int", value = 0, desc = "Gives infinite weapon clip", def = 0},
    ["sp_instant_respawn"] = {type = "int", value = 1, desc = "Instant respawn (0 - never, 1 - only before ball spawn, 2 - allways)", def = 1},
    ["sp_roundtimer_addtime"] = {type = "int", value = 0, desc = "The amount of time to add after scoring or swaping in seconds", def = 0},
    ["sp_gibigao_protection"] = {type = "int", value = 0, desc = "While enabled disables scoring if player is not blast jumping", def = 0},
    ["sp_exec_cfg"] = {type = "int", value = 0, desc = "Should the script automaticly exec streetpass_vscripts.cfg", def = 0},
    ["sp_reload_on_pass"] = {type = "int", value = 1, desc = "Allows for a reload when you get passed to or intercept the ball", def = 1},
    ["sp_passive_reload"] = {type = "int", value = 0, desc = "Enables passive reload when holding the ball", def = 0},
    ["sp_passive_reload_delay"] = {type = "float", value = 1.0, desc = "Delay between passive reloads", def = 1.0},
    ["sp_remove_intrception_protection"] = {type = "int", value = 0, desc = "Removes the protection after a steal or intercept", def = 0},
    //1.6.21
    ["sp_blitz_enable"] = { type = "int", value = 1, desc = "Enables Blitz", def = 1},
    ["sp_blitz_starttime"] = { type = "int", value = 60, desc = "Start time for blitz (seconds)", def = 60},
    ["sp_overtime_blitz_enable"] = { type = "int", value = 1, desc = "Enable blitz in overtime", def = 1},
    ["sp_overtime_enable"] = { type = "int", value = 1, desc = "Enable overtime if score difrence is equal to 0", def = 1},
    //1.7
    ["sp_stamina_enable"] = { type = "int", value = 1, desc = "Enable stamina", def = 1},
    ["sp_stamina_regen_time"] = { type = "float", value = 5.0, desc = "Time for stamina to start regenerating", def = 5.0},
    ["sp_stamina_regen_rate"] = { type = "float", value = 1.0, desc = "Rate at which stamina regenerates", def = 1.0},
    ["sp_stamina_recharge_amount"] = { type = "float", value = 150.0, desc = "Amount of meter to recharge after hitting the threshold", def = 150.0},
    ["sp_stamina_recharge_rate"] = { type = "float", value = 2.0, desc = "Rate of recharge", def = 2.0},
    ["sp_stamina_drop_threshold"] = { type = "float", value = 100.0, desc = "Threshold for dropping the ball", def = 100.0},
    ["sp_stamina_nojack_dmg_coef"] = { type = "float", value = 0.5, desc = "How much stamina to take for players not holding jack", def = 0.5},
};

::gamerules <- Entities.FindByClassname(null, "tf_gamerules");
gamerules.ValidateScriptScope();

::SetData <- function(name, value) {
    local data = gamerules.GetScriptScope();
    data[name] <- value;
}

::GetData <- function(name) {
    local data = gamerules.GetScriptScope();
    if (name in data)
        return data[name];
    else
        return null;
}

::PrintSpCvars <- function() {
    foreach (key, val in streetpassConvars) {
        error(key + " = " + val.value);
        if (val.value != val.def)
            print(" ( def. \"" + val.def + "\" )");
        printl("\n" + val.desc + "\n")
    }
}

::ResetSpCvars <- function() {
    foreach (key, val in streetpassConvars)
        val.value = val.def;
}

::sp_reset_convars <- ResetSpCvars;
::sp_help <- function() {
    local str =
    "to change a value of a convar type: script sp_convar(new_value)\n\n" +
    "sp_help\n" +
    "displays help for avaible convars\n\n" +
    "sp_reset_convars\n" +
    "resets the convars to default values\n\n";

    foreach (key, val in streetpassConvars) {
        str += key + " = " + val.value;
        if (val.value != val.def)
            str += " ( def. \"" + val.def + "\" )";
        str += "\n" + val.desc + "\n\n";
    }

    printl(str);

    //TODO: Fix cvars not printing corectly
    //it looks like mixing error and printl might have to do something with it
    //the solution above is just a bandaid with no colors :<

    // printl("to change a value of a convar type: script sp_convar(new_value)\n");
    // error("sp_help\n");
    // printl("displays help for avaible convars\n");
    // error("sp_reset_convars\n");
    // printl("resets the convars to default values\n");
    // PrintSpCvars();
}

::SetSpCvar <- function(name, value) {
    if (!(name in streetpassConvars)) {
        error(format("[STREETPASS]: %s is not an existing streetpass convar.\n", name));
        return;
    }

    switch (streetpassConvars[name].type) {
        case "int": {
            value = value.tointeger();
            if(streetpassConvars[name].value == value)
                return;

            streetpassConvars[name].value = value;
            ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "Streetpass cvar '" + name + "' changed to " + value);
            SetData("streetpassConvars", streetpassConvars);
            break;
        }
        case "float": {
            value = value.tofloat();
            if(streetpassConvars[name].value == value)
                return;

            streetpassConvars[name].value = value;
            ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "Streetpass cvar '" + name + "' changed to " + value);
            SetData("streetpassConvars", streetpassConvars);
            break;
        }
        default:
            Assert(0, "Unreachable");
    }
}

::GetSpCvar <- function(name) {
    if (!(name in streetpassConvars)) {
        error(format("[STREETPASS]: %s is not an existing streetpass convar.\n", name));
        return null;
    }

    return streetpassConvars[name].value;
}

foreach (key, val in streetpassConvars) {
    getroottable()[key] <- function(value, key=key) {
        SetSpCvar(key, value);
    }
}

::previousConvars <- GetData("streetpassConvars");
if (previousConvars != null)
    streetpassConvars = previousConvars;

const BLUE = 3;
const RED = 2;
const MAX_WEAPONS = 8;

::attackerTeam <- BLUE;
::defenseTeam <- RED;
::jackTeam <- RED;
::ballSpawned <- false;
::ballSpawnTime <- 0.0;

::ClientCommand <- Entities.CreateByClassname("point_clientcommand");
Entities.DispatchSpawn(ClientCommand);

::redGoal <- Entities.FindByName(null, "red_goal");
::blueGoal <- Entities.FindByName(null, "blue_goal");
::goal <- null;
if(redGoal == null || blueGoal == null)
{
    if(redGoal != null)
        goal = redGoal;
    else if(blueGoal != null)
        goal = blueGoal;
    else
    {
        while(goal = Entities.FindByClassname(goal, "func_passtime_goal"))
        {
            if(goal.GetName() == "visualizer")
                continue;

            if(goal.GetName() == "skip")
                continue;

            break;
        }
    }
}

//Prints a streetpass msg to chat
::PrintStreetPASS <- function(msg, player = null) {
    ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS]\x01 " + msg)
}

::visualizers <- [];

::swapZones <- [];
::swapZoneVisuals <- [];

::noBallZoneVisuals <- [];

::redSpawns <- [];
::blueSpawns <- [];

::packRange <- Convars.GetFloat("tf_passtime_pack_range");

::passtimeLogic <- Entities.FindByClassname(null, "passtime_logic");

::timer <- Entities.FindByClassname(null, "team_round_timer");

::MaxPlayers <- MaxClients().tointeger()

::matchEnded <- false;
::winnerTeam <- 0;

::instantRespawn <- true;

::jackOwner <- null;

//tf_team entities
::teams <- []

::ball_spawns <- []

//---- Stats Varibles ----
const STAT_SCORE = 0;
const STAT_ASSIST = 1;
const STAT_STEAL = 2;
const STAT_INTERCEPT = 3;
const STAT_SWAP = 4;
const STAT_KILLSTREAK = 5;
const STAT_LENGTH = 6;

::playerTable <- {}
::sideSwaps <- 0;
//------------------------

printl("------------------------");
printl("\nStreetPASS v."+VERSION);
printl("for info on streetpass convars type: script sp_help()\n");
printl("------------------------");

::GetPlayersInTeam <- function (team) {
    local numb = 0;
    for (local i = 1; i <= MaxPlayers; i++) {
        local player = PlayerInstanceFromIndex(i);
        if (player == null || player.IsPlayer() == false)
            continue;

        if (player.GetTeam() == team)
            numb++;
    }
    return numb++;
}

::SetStat <- function(playerIndex, stat, val) {
    if (!(playerIndex in playerTable)) {
        playerTable[playerIndex] <- {};
        for (local i = 0; i < STAT_LENGTH; i++)
            playerTable[playerIndex][i] <- 0;
    }

    playerTable[playerIndex][stat] = val;
}

::IncStat <- function(playerIndex, stat, by=1) {
    if (!(playerIndex in playerTable)) {
        playerTable[playerIndex] <- {};
        for (local i = 0; i < STAT_LENGTH; i++)
            playerTable[playerIndex][i] <- 0;
    }

    playerTable[playerIndex][stat] += by;
}

::GetStat <- function (playerIndex, stat) {
    if (!(playerIndex in playerTable)) {
        playerTable[playerIndex] <- {};
        for (local i = 0; i < STAT_LENGTH; i++)
            playerTable[playerIndex][i] <- 0;
    }

    return playerTable[playerIndex][stat];
}

::IsPlayerValid <- function (player) {
    if (player == null || player.IsPlayer() == false)
        return false;

    if (player.GetTeam() == 0 || player.GetTeam() == 1)
        return false;

    return true;
}

::PlayerThink <- function() {
    //Set killstreak
    NetProps.SetPropIntArray(self, "m_Shared.m_nStreaks", GetStat(self, STAT_KILLSTREAK), 0);

    //instant respawn
    if (!self.IsAlive() && (GetSpCvar("sp_instant_respawn") > 1 || GetSpCvar("sp_instant_respawn") && instantRespawn && !matchEnded))
        self.ForceRespawn();

    //removing interception effects
    if (self.InCond(Constants.ETFCond.TF_COND_PASSTIME_INTERCEPTION) && GetSpCvar("sp_remove_intrception_protection") >= 1)
        self.RemoveCondEx(Constants.ETFCond.TF_COND_PASSTIME_INTERCEPTION, true);

    if (self.InCond(Constants.ETFCond.TF_COND_BURNING))
        self.RemoveCond(Constants.ETFCond.TF_COND_BURNING);

    // removed this cuz it may couse bad things in map logic and pyro doesnt even have airblast so its not needed
    // if (self.InCond(Constants.ETFCond.TF_COND_KNOCKED_INTO_AIR))
    //     self.RemoveCond(Constants.ETFCond.TF_COND_KNOCKED_INTO_AIR);

    // if (self.InCond(Constants.ETFCond.TF_COND_AIR_CURRENT))
    //     self.RemoveCond(Constants.ETFCond.TF_COND_AIR_CURRENT);

    //infinite health
    if ((!matchEnded || self.GetTeam() == winnerTeam) && !self.IsFakeClient())
        self.SetHealth(999);

    //infinite ammo
    local weapon = self.GetActiveWeapon();
    local player_class = self.GetPlayerClass();
    if (weapon) {
        if (GetSpCvar("sp_infinite_clip") && weapon.UsesClipsForAmmo1() && (player_class == Constants.ETFClass.TF_CLASS_DEMOMAN || player_class == Constants.ETFClass.TF_CLASS_SOLDIER))
            weapon.SetClip1(weapon.GetMaxClip1());

        if (GetSpCvar("sp_infinite_clip") && weapon.GetClassname() == "tf_weapon_particle_cannon")
            NetProps.SetPropFloat(weapon, "m_flEnergy", 20.0);

        local ammo_type = NetProps.GetPropInt(weapon, "m_iPrimaryAmmoType");
        if (ammo_type > 0)
            NetProps.SetPropIntArray(self, "m_iAmmo", 999, ammo_type);

        //caber logic
        local weapon_name = weapon.GetPrintName();
        if(weapon_name == "#TF_Weapon_StickBomb" && GetSpCvar("sp_demoman_caber_recharge_time") >= 0) {
            local detonated = NetProps.GetPropInt(weapon, "m_iDetonated")
            if (detonated && self.GetScriptScope().caberTimeSet == false) {
                self.GetScriptScope().caberTime = Time();
                self.GetScriptScope().caberTimeSet = true;
            }

            if(self.GetScriptScope().caberTime + GetSpCvar("sp_demoman_caber_recharge_time") <= Time()) {
                NetProps.SetPropInt(weapon, "m_iDetonated", 0);
                self.GetScriptScope().caberTimeSet = false;
            }
        }

        if (GetSpCvar("sp_passive_reload") && weapon.GetClassname() == "tf_weapon_passtime_gun" && Time() - self.GetScriptScope().lastReload >= GetSpCvar("sp_passive_reload_delay")) {
            self.GetScriptScope().lastReload = Time();

            for (local i = 0; i < MAX_WEAPONS; i++) {
                local held_weapon = NetProps.GetPropEntityArray(self, "m_hMyWeapons", i);
                if (held_weapon == null)
                    continue;

                if (held_weapon.UsesClipsForAmmo1() && held_weapon.Clip1() != held_weapon.GetMaxClip1()) {
                    held_weapon.SetClip1(held_weapon.Clip1() + 1);
                }

                if (held_weapon.GetClassname() == "tf_weapon_particle_cannon" && NetProps.GetPropFloat(held_weapon, "m_flEnergy") < 20.0) {
                    NetProps.SetPropFloat(held_weapon, "m_flEnergy", min(20.0, NetProps.GetPropFloat(held_weapon, "m_flEnergy") + 5.0));
                }
            }
        }
    }

    //infinite shield charge
    for (local wearable = self.FirstMoveChild(); wearable != null; wearable = wearable.NextMovePeer()) {
        if (wearable.GetClassname() != "tf_wearable_demoshield")
            continue;

        local charge = NetProps.GetPropFloat(self, "m_Shared.m_flChargeMeter");

        if (!self.InCond(Constants.ETFCond.TF_COND_SHIELD_CHARGE) && charge < GetSpCvar("sp_demoman_minchargepercentage")) {
            local mincharge = GetSpCvar("sp_demoman_minchargepercentage");
            if (mincharge < 0)
                mincharge = 0
            else if (mincharge > 100)
                mincharge = 100

            NetProps.SetPropFloat(self, "m_Shared.m_flChargeMeter", mincharge);
        }

        if (GetSpCvar("sp_medic_replicates_democharge") && self.InCond(Constants.ETFCond.TF_COND_SHIELD_CHARGE)) {
            for (local i = 1; i <= MaxPlayers; i++) {
                local player = PlayerInstanceFromIndex(i);
                if (!IsPlayerValid(player) || player.entindex() == self.entindex() || player.GetTeam() != self.GetTeam())
                    continue;

                if (player.GetPlayerClass() == Constants.ETFClass.TF_CLASS_MEDIC) {
                    local self_pos = self.GetOrigin();
                    local medic_pos = player.GetOrigin();

                    if ((medic_pos - self_pos).Length() < packRange && !(player.GetFlags() & Constants.FPlayer.FL_ONGROUND) && player.GetActiveWeapon().GetClassname() == "tf_weapon_passtime_gun") {
                        player.SetAbsVelocity(self.GetAbsVelocity());
                        break;
                    }
                }
            }
        }
    }

    local overlay = "";
    if (self.GetTeam() == attackerTeam) {
        overlay += "attacker_";
    } else {
        overlay += "defender_";
    }

    if (self.GetTeam() == RED) {
        overlay += "red_overlay";
    } else if (self.GetTeam() == BLUE) {
        overlay += "blue_overlay";
    } else {
        overlay = "";
    }

    self.SetScriptOverlayMaterial("streetpass/" + overlay);

    if (GetSpCvar("sp_stamina_enable") && self.GetScriptScope().stamina_text && self.GetScriptScope().stamina_text.IsValid()) {
        local stamina = self.GetScriptScope().stamina;
        self.GetScriptScope().stamina_text.AcceptInput("SetText", stamina.tointeger().tostring(), null, null);
        if (stamina < self.GetScriptScope().stamina_max / 4.0) {
            self.GetScriptScope().stamina_text.AcceptInput("SetColor", "255 0 0", null, null);
        } else if (stamina < self.GetScriptScope().stamina_max / 2.0) {
            self.GetScriptScope().stamina_text.AcceptInput("SetColor", "255 255 0", null, null);
        } else {
            self.GetScriptScope().stamina_text.AcceptInput("SetColor", "0 255 0", null, null);
        }

        if (stamina < GetSpCvar("sp_stamina_drop_threshold")) {
            self.AddCustomAttribute("cannot pick up intelligence", 1, -1);
        } else {
            self.RemoveCustomAttribute("cannot pick up intelligence");
        }

        if (Time() - self.GetScriptScope().stamina_last_hit > GetSpCvar("sp_stamina_regen_time") || !weapon || weapon.GetClassname() != "tf_weapon_passtime_gun" || self.GetScriptScope().stamina_recharging) {
            if (Time() - self.GetScriptScope().stamina_last_regen > 0.05) {
                local rate = GetSpCvar("sp_stamina_regen_rate");
                if (self.GetScriptScope().stamina_recharging) {
                    rate = GetSpCvar("sp_stamina_recharge_rate");
                    if (self.GetScriptScope().stamina + rate >= GetSpCvar("sp_stamina_recharge_amount")) {
                        self.GetScriptScope().stamina_recharging = false;
                    }
                }
                self.GetScriptScope().stamina = self.GetScriptScope().stamina + rate;
                if (self.GetScriptScope().stamina > self.GetScriptScope().stamina_max) {
                    self.GetScriptScope().stamina = self.GetScriptScope().stamina_max;
                }
                self.GetScriptScope().stamina_last_regen = Time();
            }
        }
    }

    return -1;
}

::PlayerHasBall <- function(player) {
    if(player.GetActiveWeapon().GetClassname() == "tf_weapon_passtime_gun")
        return true;

    return false;
}

//--- TOP AREA LOGIC ---
class ProtectionArea {
    triggers = null;
    players = null;
    isActive = null;
    team = null;

    constructor(_team, triggerName)
    {
        this.triggers = [];
        this.players = [];
        this.isActive = false;
        this.team = _team;

        local t = null;
        while (t = Entities.FindByName(t, triggerName)) {
            t.ValidateScriptScope();
            t.GetScriptScope().idx <- this.triggers.len();
            this.triggers.append(t);
        }
    }

    function SetAreaTeam(_team) {
        this.team = _team;
    }

    function OnEnter() {
        if(!IsPlayerValid(activator))
            return;

        this.players.append(activator);
    }

    function OnExit() {
        if(!IsPlayerValid(activator))
            return;

        // if(activator.GetScriptScope().oldTeam == this.team && ballSpawned)
        // {
        //     this.Disable();
        // }

        for (local i = 0; i < this.players.len(); i++) {
            if(this.players[i] == activator){
                this.players.remove(i);
            }
        }
    }

    function RemovePlayer(player) {
        if(!IsPlayerValid(player))
            return;

        for (local i = 0; i < this.players.len(); i++) {
            if(this.players[i] == player){
                this.players.remove(i);
            }
        }
    }

    function AddPlayer(player) {
        if(!IsPlayerValid(player))
            return;

        this.players.append(player);
    }

    function OnTrigger(force = false) {
        if(!force)
            if(!this.isActive)
                return;

        local teleported = []
        for (local i = 0; i < this.players.len(); i++) {
            local player = this.players[i];

            if(player.GetTeam() != this.team) {
                player.ForceRespawn();
                teleported.append(player.entindex())
            }
        }
        FireScriptEvent("sp_protection_triggered", { team = this.team, players_teleprted = teleported });
    }

    function Enable() {
        if (this.isActive)
            return;

        this.isActive = true;
        FireScriptEvent("sp_protection_enabled", {});
    }

    function Disable(notify = false) {
        if (!this.isActive)
            return;

        this.isActive = false;
        FireScriptEvent("sp_protection_disabled", {});

        if (notify)
            PrintStreetPASS("Protection is no longer active!");
    }

    function PrintPlayers() {
        for (local i = 0; i < this.players.len(); i++) {
            printl(this.players[0])
        }
    }
}

::defendersProtection <- null;
::attackersProtection <- null;

::DisableProtection <- function() {
    defendersProtection.Disable();
    attackersProtection.Disable();
}

::EnableProtection <- function() {
    defendersProtection.Enable();
    attackersProtection.Enable();
}

::TriggerProtection <- function(force = false) {
    defendersProtection.OnTrigger(force);
    attackersProtection.OnTrigger(force);
}

::PrintProtectionPlayers <- function() {
    printl("attackers_prot: ")
    attackersProtection.PrintPlayers()
    printl("defenders_prot: ")
    defendersProtection.PrintPlayers()
}
//----------------------

//--- NO BALL ZONE LOGIC ---
::noBallZones <- [];
::dontSwap <- false;

::PlayerDisableBallPickup <- function() {
    if(!IsPlayerValid(activator))
        return;

    if(!PlayerHasBall(activator))
        return;

    if(activator.GetTeam() == attackerTeam)
        return;

    local activator = activator;
    activator.AddCustomAttribute("cannot pick up intelligence", 1, -1);
    dontSwap = true;

    self.AcceptInput("FireUser1", "", activator, self);
    if(defenseTeam == RED)
        self.AcceptInput("FireUser2", "", activator, self);
    else
        self.AcceptInput("FireUser3", "", activator, self);

    for (local i = 0; i < noBallZoneVisuals.len(); i++) {
        local visual = noBallZoneVisuals[i];
        visual.SetAbsOrigin(Vector(activator.GetOrigin().x, activator.GetOrigin().y, visual.GetOrigin().z));
        visual.AcceptInput("FireUser1", "", activator, self);
        //User2 and User3 are set up for team related stuff
        if (activator.GetTeam() == RED) {
            visual.AcceptInput("FireUser2", "", activator, self);
        } else {
            visual.AcceptInput("FireUser3", "", activator, self);
        }
    }
}

::PlayerEnableBallPickup <- function() {
    if(!IsPlayerValid(activator))
        return;

    activator.RemoveCustomAttribute("cannot pick up intelligence");
}
//----------------------

::StreetpassThink <- function() {
    local ball = Entities.FindByClassname(null, "passtime_ball");

    if (ball) {
        local ent = null;
        while (ent = Entities.FindByClassname(ent, "tf_projectile_healing_bolt")) {
            local dir = ball.GetOrigin() - ent.GetOrigin()
            if (dir.Length() < 16.0) {
                local owner = ent.GetOwner();
                if (owner) {
                    local name = NetProps.GetPropString(owner, "m_szNetname");
                    PrintStreetPASS(name + " splashed the ball with an arrow!");
                }
                dir.Norm();
                dir = dir.Scale(192.0);
                ball.ApplyAbsVelocityImpulse(dir);
                jackTeam = 0;
                ball.SetTeam(0);
                ent.Destroy();
                break;
            }
        }

        local radius = GetSpCvar("sp_pyro_df_splash_radius");
        ent = null;
        while (ent = Entities.FindByClassname(ent, "tf_projectile_balloffire")) {
            local dir = ball.GetOrigin() - ent.GetOrigin()
            if (dir.Length() < radius) {
                dir = ent.GetAbsAngles().Forward();
                dir.Norm();
                dir = dir.Scale(512.0);
                ball.SetPhysVelocity(dir);
                jackTeam = 0;
                ball.SetTeam(0);
                ent.Destroy();
                break;
            }
        }
    }

    for (local i = 1; i <= MaxPlayers; i++) {
        local player = PlayerInstanceFromIndex(i);
        if (IsPlayerValid(player))
            continue;

        if (player) {
            NetProps.SetPropString(player, "m_iszScriptThinkFunction", "");
            player.SetScriptOverlayMaterial("");
        }
    }
}

::min <- function(a, b) {
    return (a < b) ? a : b;
}

::max <- function(a, b) {
    return (a > b) ? a : b;
}

::IsPointInTrigger <- function(point, trigger) {
    trigger.RemoveSolidFlags(4)  // FSOLID_NOT_SOLID
    local trace =
    {
        start = point
        end   = point
        mask  = 1
    }
    TraceLineEx(trace)
    trigger.AddSolidFlags(4)

    return trace.hit && trace.enthit == trigger
}

::SwapSides <- function() {
    if (activator.GetClassname() != "passtime_ball")
        return;

    if (jackTeam != defenseTeam)
        return;

    if (jackOwner == null)
        return;

    if(dontSwap)
        return;

    FireScriptEvent("sp_swap_sides", {swaper = jackOwner, swapzone = caller, ball_pos = activator.GetOrigin(), old_defense = defenseTeam, old_attack = attackerTeam});

    //set varibles
    local hold = attackerTeam;
    attackerTeam = defenseTeam;
    defenseTeam = hold;

    defendersProtection.SetAreaTeam(defenseTeam);
    attackersProtection.SetAreaTeam(attackerTeam);

    jackTeam = 0;
    sideSwaps += 1;
    instantRespawn = true;
    ballSpawned = false;

    //swap spawn locations
    for (local i = 0; i < blueSpawns.len(); i++)
        blueSpawns[i].SetTeam(attackerTeam);

    for (local i = 0; i < redSpawns.len(); i++)
        redSpawns[i].SetTeam(defenseTeam);

    local oName = NetProps.GetPropString(PlayerInstanceFromIndex(jackOwner), "m_szNetname");
    IncStat(jackOwner, STAT_SWAP);

    //toggle goals
    if (attackerTeam == RED) {
        if(goal == null)
        {
            redGoal.AcceptInput("Enable", "", self, self);
            blueGoal.AcceptInput("Disable", "", self, self);
        }else
        {
            goal.SetTeam(RED);
        }
        PrintStreetPASS(oName+"\x01 Swaped sides! \x07FF3F3FRED \x01team is now Attacking!");
    } else {
        if(goal == null)
        {
            blueGoal.AcceptInput("Enable", "", self, self);
            redGoal.AcceptInput("Disable", "", self, self);
        }else
        {
            goal.SetTeam(BLUE);
        }
        PrintStreetPASS(oName+"\x01 Swaped sides! \x0799CCFFBLU \x01team is now Attacking!");
    }

    //set visualizers
    for (local i = 0; i < visualizers.len(); i++)
        visualizers[i].SetTeam(defenseTeam);

    //play the swap sound
    EmitSoundEx({
        sound_name = SWAP_SOUND,
        filter_type = Constants.EScriptRecipientFilter.RECIPIENT_FILTER_GLOBAL
    });

    //spawn ball and add time
    if (GetSpCvar("sp_roundtimer_addtime") && isOvertime == false) {
        timer.AcceptInput("AddTime", GetSpCvar("sp_roundtimer_addtime").tostring(), self, self);
        NetProps.SetPropInt(gamerules, "m_iRoundState", Constants.ERoundState.GR_STATE_STALEMATE);
        passtimeLogic.AcceptInput("SpawnBall", "", self, self);
        NetProps.SetPropInt(gamerules, "m_iRoundState", Constants.ERoundState.GR_STATE_RND_RUNNING);
        passtimeLogic.AcceptInput("SpawnBall", "", self, self);
        return;
    }

    if(isBlitz) {
        // local spawn = ball_spawns[RandomInt(0, ball_spawns.len() - 1)]

        for (local i = 1; i <= MaxPlayers; i++) {
            local player = PlayerInstanceFromIndex(i)

            if(!IsPlayerValid(player)) continue;

            player.ForceRespawn()
        }

        // activator.Teleport(true, spawn.GetOrigin(), false, spawn.GetAbsAngles(), false, Vector(0,0,0))
        // activator.SetTeam(0);

        // SendGlobalGameEvent("teamplay_broadcast_audio", { team = 255, sound = "Passtime.BallSpawn", additional_flags = 0, player = -1 })
    }

    passtimeLogic.AcceptInput("SpawnBall", "", self, self);
}

::GivePlayerWeapon <- function(player, classname, itemid, prevWeapon = null) {
    local weapon = Entities.CreateByClassname(classname);
    NetProps.SetPropInt(weapon, "m_AttributeManager.m_Item.m_iItemDefinitionIndex", itemid);
    NetProps.SetPropBool(weapon, "m_AttributeManager.m_Item.m_bInitialized", true);
    NetProps.SetPropBool(weapon, "m_bValidatedAttachedEntity", true);
    weapon.SetTeam(player.GetTeam());
    weapon.DispatchSpawn();
    player.Weapon_Equip(weapon);
    if (prevWeapon)
        prevWeapon.Destroy();
    return weapon;
}

::NoAirblast <- function() {
    NetProps.SetPropFloat(self, "m_flNextSecondaryAttack", Time() + 1.0);
}

::GoalAcceptInput <- function(input, value){
    if(goal == null)
    {
       if(attackerTeam == BLUE)
            blueGoal.AcceptInput(input, value, null, null);
        else
            redGoal.AcceptInput(input, value, null, null);
    }else
    {
        goal.AcceptInput(input, value, null, null);
    }
}

::roundWin <- SpawnEntityFromTable("game_round_win",{force_map_reset = true,})
::isOvertime <- false;
::isBlitz <- false;
::HandleEndgame <- function (redOverride = -1, blueOverride = -1) {
    local scoreRed = redOverride;
    if(redOverride <= -1)
        scoreRed = NetProps.GetPropInt(teams[RED], "m_nFlagCaptures")

    local scoreBlue = blueOverride;
    if(blueOverride <= -1)
        scoreBlue = NetProps.GetPropInt(teams[BLUE], "m_nFlagCaptures")

    if(abs(scoreBlue - scoreRed) > 0){
        if(scoreBlue > scoreRed) {
            NetProps.SetPropInt(roundWin, "m_iTeamNum", BLUE)
            NetProps.SetPropBool(roundWin, "m_bSwitchTeamsOnWin", true)
        }else {
            NetProps.SetPropInt(roundWin, "m_iTeamNum", RED)
            NetProps.SetPropBool(roundWin, "m_bSwitchTeamsOnWin", false)
        }

        roundWin.AcceptInput("RoundWin", "", null, null)
    }else{
        if(GetSpCvar("sp_overtime_enable") >= 1)
        {
            isOvertime = true;
            if(GetSpCvar("sp_overtime_blitz_enable") >= 1)
                ActivateBlitz(true);

            timer.AcceptInput("AddTime", "70", null, null);
            timer.AcceptInput("Pause", "", null, null);
        }else
            roundWin.AcceptInput("RoundWin", "", null, null)
    }
}
::ActivateBlitz <- function(force = false){
    if(!force)
    {
        local roundState =
        GetRoundState() == Constants.ERoundState.GR_STATE_TEAM_WIN ||
        GetRoundState() == Constants.ERoundState.GR_STATE_STALEMATE

        if(isBlitz || GetSpCvar("sp_blitz_enable") <= 0 || roundState)
            return;
    }

    NetProps.SetPropInt(passtimeLogic, "m_iBallSpawnCountdownSec", 1)
    if(!isBlitz)
        PrintStreetPASS("\x07FFFF00Blitz Active!");

    isBlitz = true;
}

::TimerThink <- function() {
    local flSecondsRemaining = NetProps.GetPropFloat(self, "m_flTimerEndTime") - Time();

    if(GetSpCvar("sp_blitz_enable") >= 1 &&
    GetSpCvar("sp_blitz_starttime") >= flSecondsRemaining
    && NetProps.GetPropInt(self, "m_nState") != 0) //setup state
    {
        ActivateBlitz();
        //we remove the think this way
        NetProps.SetPropString(self, "m_iszScriptThinkFunction", "");
    }
}
AddThinkToEnt(timer, "TimerThink");

PrecacheEntityFromTable({ classname = "info_particle_system", effect_name = "dm_low" })

local EventsID = UniqueString()
getroottable()[EventsID] <-
{
    // Cleanup events on round restart. Do not remove this event.
    OnGameEvent_scorestats_accumulated_update = function(params) { delete getroottable()[EventsID] }

    ////////// Add your events here //////////////
    //sp_swap_sides {swaper - player index, swapzone - handle, old_defense - team number, old_attack - team number, ball_pos - where the ball landed},
    //sp_pass_intercept {victim - player index, intercepter - player index},
    //sp_pass_spawn {},
    //sp_pass_splashed {splasher - player index, old_ball - team number}
    //sp_protection_enabled {} UNUSED
    //sp_protection_disabled {} UNUSED
    //sp_blast_jump { userid, playsound - bool }
    //sp_blast_jump_landed { userid }
    //sp_protection_triggered { team - team number, players_teleprted - array }

    OnGameEvent_player_death = function(params) {
        local player = GetPlayerFromUserID(params.userid);

        local weapon = Entities.FindByClassname(null, "tf_dropped_weapon");
        if (weapon)
            weapon.Destroy();

        local ammo = Entities.FindByClassname(null, "tf_ammo_pack");
        if (ammo)
            ammo.Destroy();

        player.GetScriptScope().stamina_text.Destroy();

        player.SetScriptOverlayMaterial("");
    }

    OnGameEvent_player_spawn = function(params) {
        local player = GetPlayerFromUserID(params.userid);
        player.ValidateScriptScope();
        player.GetScriptScope().caberTime <- 0;
        player.GetScriptScope().caberTimeSet <- false;
        player.GetScriptScope().lastReload <- Time();
        AddThinkToEnt(player, "PlayerThink");

        player.GetScriptScope().stamina_last_hit <- Time();
        player.GetScriptScope().stamina_last_regen <- Time();
        player.GetScriptScope().stamina_recharging <- false;
        player.GetScriptScope().stamina_max <- 500;
        player.GetScriptScope().stamina <- player.GetScriptScope().stamina_max;
        
        player.GetScriptScope().stamina_text <- SpawnEntityFromTable("point_worldtext", {
            origin       = player.GetOrigin() + Vector(0, 0, 16)
            angles       = QAngle(0, 0, 0)
            textsize  = 10
            orientation = 1
        })

        player.GetScriptScope().stamina_text.AcceptInput("SetParent", "!activator", player, null);
        player.GetScriptScope().stamina_text.AcceptInput("SetParentAttachmentMaintainOffset", "head", player, null);
    }

    OnGameEvent_player_team = function (params) {
        local player = GetPlayerFromUserID(params.userid);
        // player.GetScriptScope().oldTeam = params.oldteam;
    }

    OnGameEvent_teamplay_win_panel = function(params) {
        // SetData("streetpassConvars", streetpassConvars);

        matchEnded = true;
        winnerTeam = params.winning_team;

        //Sort players by team
        local players = [];
        for (local i = 1; i <= MaxPlayers; i++) {
            local player = PlayerInstanceFromIndex(i);
            if (IsPlayerValid(player) == false)
                continue;

            if (player.GetTeam() == params.winning_team)
                players.insert(0, i);
            else
                players.push(i);
        }

        //Print Player stats
        PrintStreetPASS("\x07FFFF00Stats: ");
        for (local i = 0; i < players.len(); i++) {
            local player = PlayerInstanceFromIndex(players[i]);

            local pName = NetProps.GetPropString(player, "m_szNetname");
            local scores = GetStat(players[i], STAT_SCORE);
            local assists = GetStat(players[i], STAT_ASSIST);
            local intercepts = GetStat(players[i], STAT_INTERCEPT);
            local steals = GetStat(players[i], STAT_STEAL);
            local swaps = GetStat(players[i], STAT_SWAP);

            local team = "FFFF00";
            if (player.GetTeam() == RED)
                team = "FF3F3F";
            else if (player.GetTeam() == BLUE)
                team = "99CCFF";

            ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK,
                "\x07"+team+pName+"\x07FFFF00 | Scores: "+scores+"\x07FFFF00 | Assists: "+assists+"\x07FFFF00 | Intercepts: "+intercepts+"\x07FFFF00 | Steals: "+steals+" | Swaps: "+swaps);
        }
        ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF00Side swaps: "+sideSwaps);

        for (local i = 1; i <= MaxPlayers; i++) {
            local player = PlayerInstanceFromIndex(i)
            if (IsPlayerValid(player) == false)
                continue;

            if (player.GetTeam() != winnerTeam)
                player.SetHealth(player.GetMaxHealth());
        }
    }

    OnGameEvent_post_inventory_application = function (params) {
        local player = GetPlayerFromUserID(params.userid);
        local removed = false;
        local last_good_weapon = null;

        //ban stock weapons
        for (local i = 0; i < MAX_WEAPONS; i++) {
            local held_weapon = NetProps.GetPropEntityArray(player, "m_hMyWeapons", i);
            if (held_weapon == null)
                continue;

            local weapon_class = held_weapon.GetClassname();
            if (weapon_class == "tf_weapon_shotgun_soldier" || weapon_class == "tf_weapon_shotgun_pyro" || weapon_class == "tf_weapon_shotgun") {
                removed = true;
                NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
                PrintStreetPASS("\x07FF0a00Shotgun is not allowed!", player);
                if (player.GetPlayerClass() == Constants.ETFClass.TF_CLASS_PYRO)
                    GivePlayerWeapon(player, "tf_weapon_flaregun", 351, held_weapon);
            } else if (weapon_class == "tf_weapon_pipebomblauncher") {
                removed = true;
                PrintStreetPASS("\x07FF0a00Stickybomb launcher is not allowed!", player);
                NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
                // NetProps.SetPropInt(player, "m_Shared.m_bShieldEquipped", 1);
            } else if (weapon_class == "tf_weapon_syringegun_medic") {
                removed = true;
                PrintStreetPASS("\x07FF0a00Syringe gun is not allowed!", player);
                NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
                GivePlayerWeapon(player, "tf_weapon_crossbow", 305, held_weapon);
            } else if (weapon_class == "tf_weapon_rocketlauncher_fireball") {
                local cooldown = GetSpCvar("sp_pyro_primary_charge_rate");
                held_weapon.AddAttribute("item_meter_charge_rate", cooldown, 0);
                AddThinkToEnt(held_weapon, "NoAirblast");
            } else if (weapon_class == "tf_weapon_flamethrower") {
                removed = true;
                PrintStreetPASS("\x07FF0a00 Flamethrower is not allowed!", player);
                NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
                local cooldown = GetSpCvar("sp_pyro_primary_charge_rate");
                local weapon = GivePlayerWeapon(player, "tf_weapon_rocketlauncher_fireball", 1178, held_weapon);
                weapon.AddAttribute("item_meter_charge_rate", cooldown, 0);
                AddThinkToEnt(weapon, "NoAirblast");
            } else if (weapon_class == "tf_weapon_katana") {
                held_weapon.AddAttribute("honorbound", casti2f(0), 0);
            } else {
                if (!last_good_weapon)
                   last_good_weapon = held_weapon;
            }
        }

        //set active weapon to a valid one after removal
        if (removed && last_good_weapon != null)
            player.Weapon_Switch(last_good_weapon);
    }

    OnGameEvent_tf_game_over = function(params) {
        matchEnded = false;
    }

    //We do this to update the winlimit on new round
    //cuz if blue team wins and we swap teams it will display incorectly
    //this also will work with these events: teamplay_round_start, stop_watch_changed, winpanel_show_scores <- the last 2 are not mentioned on valve wiki (i wonder why?)
    //https://github.com/ValveSoftware/source-sdk-2013/blob/39f6dde8fbc238727c020d13b05ecadd31bda4c0/src/game/client/tf/tf_hud_match_status.cpp#L232
    OnGameEvent_teamplay_round_active = function (params) {
        SendGlobalGameEvent("stop_watch_changed", {})
    }

    OnGameEvent_recalculate_holidays = function(params) {
        if(GetSpCvar("sp_exec_cfg") == 0)
            return;

        if (IsDedicatedServer()) {
            SendToConsole("exec streetpass_vscripts.cfg");
        }
        else {
            SendToServerConsole("exec streetpass_vscripts.cfg");
        }
    }

    OnScriptHook_OnTakeDamage = function(params) {
        local victim = params.const_entity;
        local attacker = params.attacker;

        if (params.weapon != null) {
            local weapon = params.weapon;
            if (weapon.GetClassname() == "tf_weapon_flaregun") {
                local splash_radius = GetSpCvar("sp_pyro_detonator_splash_radius");
                if (victim.GetClassname() == "passtime_ball") {
                    if (victim.GetTeam() != 0 && (!splash_radius || (params.inflictor.GetOrigin() - victim.GetOrigin()).Length() > splash_radius)) {
                        params.damage = 0;
                        params.const_base_damage = 0;
                        params.early_out = true;
                        return;
                    }
                } else {
                    local pushmult = GetSpCvar("sp_pyro_detonator_knockback_mult");
                    if (!pushmult)
                        pushmult = 1.0;
                    params.damage *= pushmult;
                }
            }
        }

        if (IsPlayerValid(victim) && IsPlayerValid(attacker) && victim != attacker) {
            local weapon = victim.GetActiveWeapon();

            local dmg = 0;
            if (weapon && weapon.GetClassname() == "tf_weapon_passtime_gun") {
                dmg = params.damage;
            } else {
                dmg = params.damage * GetSpCvar("sp_stamina_nojack_dmg_coef");
            }

            victim.GetScriptScope().stamina_last_hit <- Time();
            victim.GetScriptScope().stamina = victim.GetScriptScope().stamina - dmg;
            if (victim.GetScriptScope().stamina < 0) {
                victim.GetScriptScope().stamina = 0;
            }
            if (victim.GetScriptScope().stamina <= GetSpCvar("sp_stamina_drop_threshold")) {
                victim.GetScriptScope().stamina_recharging = true;
            }
        }

        if (victim.GetClassname() == "passtime_ball" && IsPlayerValid(attacker)) {
            jackTeam = 0;
            if (victim.GetTeam() != 0)
                FireScriptEvent("sp_pass_splashed", {splasher = attacker.entindex(), old_ball = victim.GetTeam()});
            return;
        }

        if (!IsPlayerValid(victim) || !GetSpCvar("sp_medic_replicates_blast_jump"))
            return;

        for (local i = 1; i <= MaxPlayers; i++) {
            local player = PlayerInstanceFromIndex(i);
            if (!IsPlayerValid(player) || player.entindex() == victim.entindex() || player.GetTeam() != victim.GetTeam())
                continue;

            if (player.GetPlayerClass() == Constants.ETFClass.TF_CLASS_MEDIC) {
                local victim_pos = victim.GetOrigin();
                local medic_pos = player.GetOrigin();

                if ((medic_pos - victim_pos).Length() < packRange && !(player.GetFlags() & Constants.FPlayer.FL_ONGROUND) && player.GetActiveWeapon().GetClassname() == "tf_weapon_passtime_gun") {
                    if (GetSpCvar("sp_medic_replicates_caber") && params.weapon != null && params.weapon.GetClassname() == "tf_weapon_stickbomb") {
                        local demo_vel = victim.GetAbsVelocity();
                        local medic_vel = player.GetAbsVelocity();
                        medic_vel.z = 620;
                        player.SetAbsVelocity(medic_vel);
                    } else {
                        local force = params.damage_force.Scale(1.0 / pow(2, 5));
                        player.SetAbsVelocity(player.GetAbsVelocity() + force);
                    }
                    break;
                }
            }
        }
    }

    OnGameEvent_pass_get = function(params) {
        local owner = PlayerInstanceFromIndex(params.owner);
        jackTeam = owner.GetTeam();
        dontSwap = false;

        GoalAcceptInput("Enable", "");

        owner.GetScriptScope().lastReload = Time();
    }

    OnGameEvent_pass_pass_caught = function(params) {
        // Intercept check for catchers team
        // passer (short)
        // catcher (short)
        // dist (float)
        // duration (float)

        local passer = PlayerInstanceFromIndex(params.passer);
        local catcher = PlayerInstanceFromIndex(params.catcher);
        dontSwap = false;

        EntFireByHandle(ClientCommand, "Command", "r_screenoverlay off", 0.1, catcher, null);

        GoalAcceptInput("Enable", "");

        if (catcher.GetTeam() != passer.GetTeam()) {
            local pName = NetProps.GetPropString(passer, "m_szNetname");
            local cName = NetProps.GetPropString(catcher, "m_szNetname");
            IncStat(params.catcher, STAT_INTERCEPT);
            FireScriptEvent("sp_pass_intercept", {victim = params.passer, intercepter = params.catcher});

            PrintStreetPASS(cName+"\x01 Intercepted "+pName+"\x01 throw!");
        }

        catcher.GetScriptScope().lastReload = Time();

        local weapon = catcher.GetActiveWeapon();
        if (weapon) {
            if (GetSpCvar("sp_reload_on_pass") && weapon.UsesClipsForAmmo1() && weapon.Clip1() != weapon.GetMaxClip1())
                weapon.SetClip1(weapon.Clip1() + 1);

            if (GetSpCvar("sp_reload_on_pass") && weapon.GetClassname() == "tf_weapon_particle_cannon" && NetProps.GetPropFloat(weapon, "m_flEnergy") != 100)
                NetProps.SetPropFloat(weapon, "m_flEnergy", min(20.0, NetProps.GetPropFloat(weapon, "m_flEnergy") + 5.0));
        }
    }

    OnGameEvent_pass_free = function(params) {
        local owner = PlayerInstanceFromIndex(params.owner);
        jackOwner = params.owner;
        jackTeam = owner.GetTeam();

        if(owner.GetPlayerClass() == Constants.ETFClass.TF_CLASS_PYRO || owner.GetPlayerClass() == Constants.ETFClass.TF_CLASS_MEDIC)
            return;

        if(GetSpCvar("sp_gibigao_protection") >= 1 && owner.InAirDueToExplosion() == false)
        {
            GoalAcceptInput("Disable", "");
        }
    }

    OnGameEvent_pass_score = function(params) {
        // scorer (short)
        // assister (short)
        // points (byte)
        if (GetSpCvar("sp_roundtimer_addtime") && isOvertime == false)
            timer.AcceptInput("AddTime", GetSpCvar("sp_roundtimer_addtime").tostring(), self, self);

        local scorer = PlayerInstanceFromIndex(params.scorer);
        local sName = NetProps.GetPropString(scorer, "m_szNetname");
        IncStat(params.scorer, STAT_SCORE);

        IncStat(scorer, STAT_KILLSTREAK, 2);
        instantRespawn = true;
        ballSpawned = false;

        if (params.assister < 0) {
            PrintStreetPASS(sName+"\x01 Scored!");
        } else {
            local assister = PlayerInstanceFromIndex(params.assister);
            local aName = NetProps.GetPropString(assister, "m_szNetname");
            IncStat(params.assister, STAT_ASSIST);

            IncStat(assister, STAT_KILLSTREAK, 1);

            PrintStreetPASS(sName+"\x01 Scored! Assisted by "+aName);
        }

        //we do this special check so we can swap teams correctly
        //the netprops of scores do not update until some time after so we need to check -1 of the max score ammount
        if(isOvertime ||
            NetProps.GetPropInt(teams[scorer.GetTeam()], "m_nFlagCaptures") >= Convars.GetInt("tf_passtime_scores_per_round") - 1){
            local redScore = 0;
            local blueScore = 0;

            if(scorer.GetTeam() == RED)
                redScore = 1;
            else
                blueScore = 1;

            HandleEndgame(redScore, blueScore);
        }
    }

    OnGameEvent_pass_ball_stolen = function(params) {
        // Melee steal
        // victim (short)
        // attacker (short)
        local victim = PlayerInstanceFromIndex(params.victim);
        local attacker = PlayerInstanceFromIndex(params.attacker);
        local vName = NetProps.GetPropString(victim, "m_szNetname");
        local aName = NetProps.GetPropString(attacker, "m_szNetname");

        EntFireByHandle(ClientCommand, "Command", "r_screenoverlay off", 0.1, attacker, null);

        GoalAcceptInput("Enable", "");

        if (victim.GetTeam() == attacker.GetTeam())
            return;

        IncStat(params.attacker, STAT_STEAL);
        attacker.GetScriptScope().lastReload = Time();

        PrintStreetPASS(aName+"\x01 Stole the ball from "+vName+"\x01!");
    }

    OnGameEvent_teamplay_broadcast_audio = function(params) {
        if (params.sound == "Passtime.BallSpawn") {
            instantRespawn = false;
            FireScriptEvent("sp_pass_spawn", {});

            ballSpawned = true;
            ballSpawnTime = Time();

            TriggerProtection(true);
        }

        local stopSound = false

        if(params.sound == "Game.Overtime" && !isOvertime)
            stopSound = true

        if(params.sound == "Announcer.RoundBegins1seconds" && isOvertime)
            stopSound = true

        if(stopSound) {
            for (local i = 0; i <= MaxPlayers; i++) {
                local player = PlayerInstanceFromIndex(i);

                if(player != null)
                    StopSoundOn(params.sound, player)
            }
        }

    }

    //---set up for blast jump events---
    OnGameEvent_rocket_jump = function (params) {
        FireScriptEvent("sp_blast_jump", params);
    }

    OnGameEvent_sticky_jump = function (params) {
        FireScriptEvent("sp_blast_jump", params);
    }

    OnGameEvent_rocket_jump_landed = function (params) {
        FireScriptEvent("sp_blast_jump_landed", params);
    }

    OnGameEvent_sticky_jump_landed = function (params) {
        FireScriptEvent("sp_blast_jump_landed", params);
    }
    //----------------------------------

    // OnScriptEvent_sp_blast_jump = function (params) {
    //     local player = GetPlayerFromUserID(params.userid);
    // }

    // OnScriptEvent_sp_blast_jump_landed = function (params) {
    //     local player = GetPlayerFromUserID(params.userid);
    // }

    OnScriptEvent_sp_swap_sides = function (params) {
        local swaper = PlayerInstanceFromIndex(params.swaper);
        for (local i = 0; i < swapZoneVisuals.len(); i++) {
            local visual = swapZoneVisuals[i];
            visual.SetAbsOrigin(Vector(params.ball_pos.x, params.ball_pos.y, visual.GetOrigin().z));
            visual.AcceptInput("FireUser1", "", swaper, params.swapzone);
            params.swapzone.AcceptInput("FireUser1", "", swaper, params.swapzone);
            //User2 and User3 are set up for team related stuff
            if(params.old_defense == RED)
            {
                visual.AcceptInput("FireUser2", "", swaper, params.swapzone);
                params.swapzone.AcceptInput("FireUser2", "", swaper, params.swapzone);
            }
            else
            {
                visual.AcceptInput("FireUser3", "", swaper, params.swapzone);
                params.swapzone.AcceptInput("FireUser3", "", swaper, params.swapzone);
            }
        }
    }

    OnGameEvent_player_disconnect = function (params) {
        local player = GetPlayerFromUserID(params.userid);
        defendersProtection.RemovePlayer(player);
        attackersProtection.RemovePlayer(player);
    }
}

function OnPostSpawn()
{
    local spawn = null;
    while (spawn = Entities.FindByClassname(spawn, "info_player_teamspawn")) {
        if (spawn.GetTeam() == BLUE)
            blueSpawns.append(spawn);
        else
            redSpawns.append(spawn);
    }

    local noBall = null;
    while (noBall = Entities.FindByName(noBall, "sp_no_ball_zone")) {
        EntityOutputs.AddOutput(noBall, "OnStartTouch", "streetpass_script", "RunScriptCode", "PlayerDisableBallPickup()", 0, -1);
        EntityOutputs.AddOutput(noBall, "OnEndTouch", "streetpass_script", "RunScriptCode", "PlayerEnableBallPickup()", 0.5, -1);
        noBallZones.append(noBall);
    }

    local visual = null;
    while (visual = Entities.FindByName(visual, "visualizer")) {
        visualizers.append(visual);
    }

    local swapzone = null;
    while (swapzone = Entities.FindByName(swapzone, "sp_swapzone")) {
        EntityOutputs.AddOutput(swapzone, "OnStartTouch", "streetpass_script", "RunScriptCode", "SwapSides()", 0, -1);
        swapZones.append(swapzone);
    }

    local szVisual = null
    while (szVisual = Entities.FindByName(szVisual, "sp_swapzone_visual")) {
        swapZoneVisuals.append(szVisual);
    }

    local szVisual = null
    while (szVisual = Entities.FindByName(szVisual, "sp_no_ball_zone_visual")) {
        noBallZoneVisuals.append(szVisual);
    }

    defendersProtection = ProtectionArea(defenseTeam, "sp_defenders_protection");
    for (local i = 0; i < defendersProtection.triggers.len(); i++) {
        local t = defendersProtection.triggers[i];
        EntityOutputs.AddOutput(t, "OnStartTouch", "streetpass_script", "RunScriptCode", "defendersProtection.OnEnter()", 0, -1);
        EntityOutputs.AddOutput(t, "OnEndTouch", "streetpass_script", "RunScriptCode", "defendersProtection.OnExit()", 0, -1);

        //We do this so we can capture everyone in the trigger instantly
        t.AcceptInput("Disable", "", null, null);
        EntFireByHandle(t, "Enable", "", 0.1, null, null)
    }

    attackersProtection = ProtectionArea(attackerTeam, "sp_attackers_protection");
    for (local i = 0; i < attackersProtection.triggers.len(); i++) {
        local t = attackersProtection.triggers[i];
        EntityOutputs.AddOutput(t, "OnStartTouch", "streetpass_script", "RunScriptCode", "attackersProtection.OnEnter()", 0, -1);
        EntityOutputs.AddOutput(t, "OnEndTouch", "streetpass_script", "RunScriptCode", "attackersProtection.OnExit()", 0, -1);

        //We do this so we can capture everyone in the trigger instantly
        t.AcceptInput("Disable", "", null, null);
        EntFireByHandle(t, "Enable", "", 0.1, null, null)
    }

    local team = Entities.FindByClassname(null, "tf_team");
    while(team)
    {
        teams.append(team);
        team = Entities.FindByClassname(team, "tf_team");
    }

    local bSpawn = Entities.FindByClassname(null, "info_passtime_ball_spawn");
    while(bSpawn)
    {
        ball_spawns.append(bSpawn);
        bSpawn = Entities.FindByClassname(bSpawn, "info_passtime_ball_spawn");
    }

    EntityOutputs.AddOutput(timer, "OnFinished", "streetpass_script", "RunScriptCode", "HandleEndgame()", 0, -1)
    AddThinkToEnt(passtimeLogic, "StreetpassThink");
}

local EventsTable = getroottable()[EventsID]
foreach (name, callback in EventsTable) EventsTable[name] = callback.bindenv(this)
__CollectGameEventCallbacks(EventsTable)