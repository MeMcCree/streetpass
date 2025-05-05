//Streetpass gamemode - made by BtC/BlaxorTheCat https://steamcommunity.com/id/BlaxorTheCat/ and Envy https://steamcommunity.com/id/Envy-Chan/
//maps using this gamemode use the sp_ prefix

const SWAP_SOUND = "coach/coach_look_here.wav";
PrecacheSound(SWAP_SOUND);

Convars.SetValue("tf_passtime_ball_reset_time", 99999);
Convars.SetValue("tf_passtime_powerball_threshold", 10000);
Convars.SetValue("tf_passtime_powerball_passpoints", 1);
Convars.SetValue("tf_passtime_powerball_decayamount", 99999);

// StreetPASS convars
::streetpassConvars <- {
    ["sp_medic_replicates_democharge"] = {type = "int", value = 0, desc = "Allows the medic to mimic demomans shield charge while holding the ball", def = 0},
    ["sp_medic_replicates_caber"] = {type = "int", value = 1, desc = "Allows the medic to mimic demomans caber jumps while holding the ball", def = 1},
    ["sp_medic_replicates_blast_jump"] = {type = "int", value = 1, desc = "Allows the medic to mimic blast jumps while holding the ball", def = 1},
    ["sp_demoman_minchargepercentage"] = {type = "float", value = 75.0, desc = "The % that the demomans shield will recharge to after a charge (0-100)", def = 75.0},
    ["sp_demoman_infinitecaber"] = {type = "int", value = 1, desc = "Gives demoman infinite caber charges", def = 1}, 
    ["sp_pyro_airblast_charge_rate"] = {type = "float", value = 3.0, desc = "", def = 3.0},
    ["sp_pyro_detonator_knockback_mult"] = {type = "float", value = 1.5, desc = "", def = 1.5},
    ["sp_pyro_detonator_splash_radius"] = {type = "float", value = 8.0, desc = "", def = 8.0},
    ["sp_infinite_clip"] = {type = "int", value = 1, desc = "Gives infinite weapon clip", def = 1},
    ["sp_instant_respawn"] = {type = "int", value = 1, desc = "Instant respawn (0 - never, 1 - only before ball spawn, 2 - allways)", def = 1},
    ["sp_roundtimer_addtime"] = {type = "int", value = 240, desc = "The amount of time to add after scoring or swaping in seconds", def = 240},
    ["sp_top_protection_time"] = {type = "int", value = 10, desc = "The amount of time before you can jump onto the mid platform as a defender", def = 10},
};

::gamerules <- Entities.FindByClassname(null, "tf_gamerules");
gamerules.ValidateScriptScope();

::SetData <- function(name, value)
{
    local data = gamerules.GetScriptScope();
    data[name] <- value;
}

::GetData <- function(name)
{
    local data = gamerules.GetScriptScope();
    if(name in data)
        return data[name];
    else
        return null;
}

::PrintSpCvars <- function()
{
    foreach (key, val in streetpassConvars)
    {
        error(key + " = " + val.value);
        if(val.value != val.def)
        {
            print(" ( def. \"" + val.def + "\" )");
        }
        printl("\n" + val.desc + "\n")
    }
}

::ResetSpCvars <- function()
{
    foreach (key, val in streetpassConvars)
    {
        val.value = val.def;
    }
}

::sp_reset_convars <- ResetSpCvars;
::sp_help <- function() 
{ 
    printl("to change a value of a convar type: script sp_convar(new_value)\n");
    error("sp_help\n");
    printl("displays help for avaible convars\n");
    error("sp_reset_convars\n");
    printl("resets the convars to default values\n");
    PrintSpCvars(); 
}

::SetSpCvar <- function(name, value)
{
    if (!(name in streetpassConvars))
    {
        error(format("[STREETPASS]: %s is not an existing streetpass convar.\n", name));
        return;
    }

    switch(streetpassConvars[name].type)
    {
        case "int":
        {
            value = value.tointeger();
            streetpassConvars[name].value = value;
            break;
        }
        case "float":
        {
            value = value.tofloat();
            streetpassConvars[name].value = value;
            break;
        }
        default:
        {
            Assert(0, "Unreachable");
        }
    }
}

::GetSpCvar <- function(name)
{
    if (!(name in streetpassConvars))
    {
        error(format("[STREETPASS]: %s is not an existing streetpass convar.\n", name));
        return null;
    }

    return streetpassConvars[name].value;
}

foreach (key, val in streetpassConvars)
{
    getroottable()[key] <- function(value, key=key)
    {
        SetSpCvar(key, value);
    }
}

::previousConvars <- GetData("streetpassConvars");
if(previousConvars != null)
{
    streetpassConvars = previousConvars;
}

const BLUE = 3;
const RED = 2;
const VERSION = "1.4.11";
const MAX_WEAPONS = 8;

::attackerTeam <- BLUE;
::defenseTeam <- RED;
::jackTeam <- BLUE;
::ballSpawned <- false;
::ballSpawnTime <- 0.0;
::topProtected <- false;

::redGoal <- Entities.FindByName(null, "red_goal");
::blueGoal <- Entities.FindByName(null, "blue_goal");
::swapZone <- Entities.FindByName(null, "streetpass_swapzone");
::topAreaTrigger <- Entities.FindByName(null, "top_area_trigger");

::redSpawns <- [];
::blueSpawns <- [];

::packRange <- Convars.GetFloat("tf_passtime_pack_range");

::passtimeLogic <- Entities.FindByClassname(null, "passtime_logic");
passtimeLogic.KeyValueFromFloat("ball_spawn_countdown", 8);

::timer <- Entities.FindByClassname(null, "team_round_timer");

::MaxPlayers <- MaxClients().tointeger()

::matchEnded <- false;
::winnerTeam <- 0;

::instantRespawn <- true;

::jackOwner <- null;

//---- Stats Varibles ----
const STAT_SCORE = 0;
const STAT_ASSIST = 1;
const STAT_STEAL = 2;
const STAT_INTERCEPT = 3;
const STAT_SWAP = 4;
const STAT_KILLSTREAK = 5;
const STAT_BLOCKDAMAGETICK = 6;
const STAT_LENGTH = 7;

::playerTable <- {}
::sideSwaps <- 0;
//------------------------

printl("------------------------");
printl("\nStreetPASS v."+VERSION);
printl("for info on streetpass convars type: script sp_help()\n");
printl("------------------------");

::SetStat <- function (playerIndex, stat, val)
{
    if(!(playerIndex in playerTable))
    {
        playerTable[playerIndex] <- {};
        for(local i = 0; i < STAT_LENGTH; i++)
        {
            playerTable[playerIndex][i] <- 0;
        }
    }

    playerTable[playerIndex][stat] = val;
}

::IncStat <- function (playerIndex, stat, by=1)
{
    if(!(playerIndex in playerTable))
    {
        playerTable[playerIndex] <- {};
        for(local i = 0; i < STAT_LENGTH; i++)
        {
            playerTable[playerIndex][i] <- 0;
        }
    }

    playerTable[playerIndex][stat] += by;
}

::GetStat <- function (playerIndex, stat)
{
    if(!(playerIndex in playerTable))
    {
        playerTable[playerIndex] <- {};
        for(local i = 0; i < STAT_LENGTH; i++)
        {
            playerTable[playerIndex][i] <- 0;
        }
    }

    return playerTable[playerIndex][stat];
}

::IsPlayerValid <- function (player)
{
    if (player == null || player.IsPlayer() == false)
    {
        return false;
    }

    if (player.GetTeam() == 0 || player.GetTeam() == 1)
    {
        return false;
    }

    return true;
}

::PlayerThink <- function ()
{
    //Set killstreak
    NetProps.SetPropIntArray(self, "m_Shared.m_nStreaks", GetStat(self, STAT_KILLSTREAK), 0);

    //instant respawn
    if(!self.IsAlive() && (GetSpCvar("sp_instant_respawn") > 1 || GetSpCvar("sp_instant_respawn") && instantRespawn))
    {
        self.ForceRespawn();
    }

    //removing interception effects
    if(self.InCond(Constants.ETFCond.TF_COND_PASSTIME_INTERCEPTION))
    {
        self.RemoveCondEx(Constants.ETFCond.TF_COND_PASSTIME_INTERCEPTION, true);
    }

    if(self.InCond(Constants.ETFCond.TF_COND_BURNING))
    {
        self.RemoveCond(Constants.ETFCond.TF_COND_BURNING);
    }

    //infinite health
    if((!matchEnded || self.GetTeam() == winnerTeam) && !self.IsFakeClient())
    {
        self.SetHealth(999);
    }

    //infinite ammo
    local weapon = self.GetActiveWeapon();
    local player_class = self.GetPlayerClass();
    if(weapon != null)
    {
        if(GetSpCvar("sp_infinite_clip") && weapon.UsesClipsForAmmo1() && (player_class == Constants.ETFClass.TF_CLASS_DEMOMAN || player_class == Constants.ETFClass.TF_CLASS_SOLDIER))
        {
            weapon.SetClip1(weapon.GetMaxClip1());
        }

        if(GetSpCvar("sp_infinite_clip") && weapon.GetClassname() == "tf_weapon_particle_cannon")
        {
            NetProps.SetPropFloat(weapon, "m_flEnergy", 100.0);
        }
        
        local ammo_type = NetProps.GetPropInt(weapon, "m_iPrimaryAmmoType");
        if(ammo_type > 0)
        {
            NetProps.SetPropIntArray(self, "m_iAmmo", 999, ammo_type);
        }
    }

    if(GetSpCvar("sp_demoman_infinitecaber"))
    {
        for (local i = 0; i < MAX_WEAPONS; i++)
        {
            local held_weapon = NetProps.GetPropEntityArray(self, "m_hMyWeapons", i);
            if(held_weapon == null)
            {
                continue;
            }
            
            //recharging caber
            local weapon_name = held_weapon.GetPrintName();
            if(weapon_name == "#TF_Weapon_StickBomb")
            {
                local detonated = NetProps.GetPropInt(held_weapon, "m_iDetonated")
                if(detonated)
                {
                    NetProps.SetPropInt(held_weapon, "m_iDetonated", 0)
                }
                break;
            }
        }
    }

    //infinite shield charge
    for(local wearable = self.FirstMoveChild(); wearable != null; wearable = wearable.NextMovePeer())
    {
        if(wearable.GetClassname() != "tf_wearable_demoshield")
        {
            continue;
        }
        local charge = NetProps.GetPropFloat(self, "m_Shared.m_flChargeMeter");

        if(!self.InCond(Constants.ETFCond.TF_COND_SHIELD_CHARGE) && charge < GetSpCvar("sp_demoman_minchargepercentage"))
        {
            local mincharge = GetSpCvar("sp_demoman_minchargepercentage");
            if(mincharge < 0)
            {
                mincharge = 0
            }else if(mincharge > 100)
            {
                mincharge = 100
            }
            NetProps.SetPropFloat(self, "m_Shared.m_flChargeMeter", mincharge);
        }

        if(GetSpCvar("sp_medic_replicates_democharge") && self.InCond(Constants.ETFCond.TF_COND_SHIELD_CHARGE))
        {
            for(local i = 1; i <= MaxPlayers; i++)
            {
                local player = PlayerInstanceFromIndex(i);
                if(!IsPlayerValid(player) || player.entindex() == self.entindex() || player.GetTeam() != self.GetTeam())
                {
                    continue;
                }

                if(player.GetPlayerClass() == Constants.ETFClass.TF_CLASS_MEDIC)
                {
                    local self_pos = self.GetOrigin();
                    local medic_pos = player.GetOrigin();

                    if((medic_pos - self_pos).Length() < packRange && !(player.GetFlags() & Constants.FPlayer.FL_ONGROUND) && player.GetActiveWeapon().GetClassname() == "tf_weapon_passtime_gun")
                    {
                        player.SetAbsVelocity(self.GetAbsVelocity());
                        break;
                    }
                }
            }
        }
    }

    //crossbow bolt ball collisions
    if(self.entindex() == 1)
    {

        if(topAreaTrigger != null && GetSpCvar("sp_top_protection_time") != 0 && ballSpawned && topProtected && (Time() - ballSpawnTime > GetSpCvar("sp_top_protection_time")))
        {
            topAreaTrigger.AcceptInput("Disable", "", null, null);
            topProtected = false;
            FireScriptEvent("sp_top_protection_disabled", {});
            ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS]\x01 Top area is no longer protected!");
        }

        local ent = null;
        while(ent = Entities.FindByClassname(ent, "tf_projectile_healing_bolt"))
        {
            local ball = Entities.FindByClassname(null, "passtime_ball");
            
            if(ball /*&& jackTeam != 0*/ )
            {
                local dir = ball.GetOrigin() - ent.GetOrigin()
                if(dir.Length() < 16.0)
                {
                    local owner = ent.GetOwner();
                    printl(owner);
                    if(owner != null)
                    {
                        local name = NetProps.GetPropString(owner, "m_szNetname");
                        ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS]\x01 " + name + " splashed the ball with an arrow!");
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
        }
    }

    return -1;
}

::min <- function(a, b)
{
    if(a < b)
    {
        return a;
    } else
    {
        return b;
    }
}

::max <- function(a, b)
{
    if(a > b)
    {
        return a;
    } else
    {
        return b;
    }
}

::PlayerLeaveTopAreaThink <- function ()
{
    local plmins = self.GetOrigin() + self.GetPlayerMins();
    local plmaxs = self.GetOrigin() + self.GetPlayerMaxs();

    local mins = topAreaTrigger.GetCenter() + topAreaTrigger.GetBoundingMins();
    local maxs = topAreaTrigger.GetCenter() + topAreaTrigger.GetBoundingMaxs();

    local x = max(plmins.x, mins.x);
    local xx = min(plmaxs.x, maxs.x);
    local y = max(plmins.y, mins.y);
    local yy = min(plmaxs.y, maxs.y);
    local z = max(plmins.z, mins.z);
    local zz = min(plmaxs.z, maxs.z);

    if(zz < z || yy < y || xx < x)
    {
        AddThinkToEnt(self, "PlayerThink");
        return -1;
    }

    if(self.InCond(Constants.ETFCond.TF_COND_SHIELD_CHARGE))
    {
        self.RemoveCond(Constants.ETFCond.TF_COND_SHIELD_CHARGE);
    }

    NetProps.SetPropEntity(self, "m_hGroundEntity", null);
    self.RemoveFlag(Constants.FPlayer.FL_ONGROUND);
    local pos = topAreaTrigger.GetCenter();
    local vel = self.GetOrigin() - pos;
    vel.Norm();
    vel = vel.Scale(9999);
    vel.z = self.GetAbsVelocity().z;
    self.SetAbsVelocity(vel);

    return -1;
}

::SwapSides <- function()
{
    if(activator.GetClassname() != "passtime_ball")
    {
        return;
    }

    if(jackTeam != defenseTeam)
    {
        return;
    }

    if(jackOwner == null)
    {
        return;
    }

    FireScriptEvent("sp_swap_sides", {swaper = jackOwner, old_defense = defenseTeam, old_attack = attackerTeam});

    //set varibles
    local hold = attackerTeam;
    attackerTeam = defenseTeam;
    defenseTeam = hold;
    jackTeam = 0;
    sideSwaps += 1;
    instantRespawn = true;
    ballSpawned = false;

    if(topAreaTrigger != null && GetSpCvar("sp_top_protection_time") != 0)
    {
        topProtected = true;
        topAreaTrigger.AcceptInput("Enable", "", null, null);
        FireScriptEvent("sp_top_protection_enabled", {});
    }

    //swap spawn locations
    for (local i = 0; i < blueSpawns.len(); i++)
    {
        blueSpawns[i].SetTeam(attackerTeam);
    }

    for (local i = 0; i < redSpawns.len(); i++)
    {
        redSpawns[i].SetTeam(defenseTeam);
    }

    local oName = NetProps.GetPropString(PlayerInstanceFromIndex(jackOwner), "m_szNetname");
    IncStat(jackOwner, STAT_SWAP);

    //toggle goals
    if(attackerTeam == RED)
    {
        redGoal.AcceptInput("Enable", "", self, self);
        blueGoal.AcceptInput("Disable", "", self, self);
        ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS] \x01"+oName+"\x01 Swaped sides! \x07FF3F3FRED \x01team is now Attacking!");
    }else
    {
        blueGoal.AcceptInput("Enable", "", self, self);
        redGoal.AcceptInput("Disable", "", self, self);
        ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS] \x01"+oName+"\x01 Swaped sides! \x0799CCFFBLU \x01team is now Attacking!");
    }

    //play the swap sound
    EmitSoundEx(
        {
            sound_name = SWAP_SOUND,
            filter_type = Constants.EScriptRecipientFilter.RECIPIENT_FILTER_GLOBAL
        });

    //spawn ball and add time
    if(GetSpCvar("sp_roundtimer_addtime"))
    {
        timer.AcceptInput("AddTime", GetSpCvar("sp_roundtimer_addtime").tostring(), self, self);
    }

    passtimeLogic.AcceptInput("SpawnBall", "", self, self);
}

::PlayerSwapTeam <- function(player)
{
    if(IsPlayerValid(player) == false) { return; }

    local newTeam = 0
    if (player.GetTeam() == RED)
    {
        newTeam = BLUE
    } else {
        newTeam = RED
    }

    player.ForceChangeTeam(newTeam, true);
}

::PlayerLeftTop <- function()
{
    if(matchEnded)
    {
        return;
    }

    if(activator.GetTeam() != attackerTeam)
    {
        return;
    }

    if(ballSpawned)
    {
        topAreaTrigger.AcceptInput("Disable", "", null, null);
        topProtected = false;
        FireScriptEvent("sp_top_protection_disabled", {});
    }
}

::PlayerEnteredTop <- function()
{
    if(activator.GetTeam() != attackerTeam)
    {
        AddThinkToEnt(activator, "PlayerLeaveTopAreaThink");
    }
}

local EventsID = UniqueString()
getroottable()[EventsID] <-
{
    // Cleanup events on round restart. Do not remove this event.
    OnGameEvent_scorestats_accumulated_update = function(params) { delete getroottable()[EventsID] }

    ////////// Add your events here //////////////
    //sp_swap_sides {swaper - player index, old_defense - team number, old_attack - team number}, 
    //sp_pass_intercept {victim - player index, intercepter - player index}, 
    //sp_pass_spawn {}, 
    //sp_pass_splashed {splasher - player index, old_ball - team number} 
    //OnScriptEvent_sp_top_protection_enabled {} 
    //OnScriptEvent_sp_top_protection_disabled {} 

    OnGameEvent_player_death = function(params)
    {
        local weapon = Entities.FindByClassname(null, "tf_dropped_weapon");
        if(weapon)
            weapon.Destroy();

        local ammo = Entities.FindByClassname(null, "tf_ammo_pack");
        while(ammo)
        {
            if(ammo.GetOwner() == PlayerInstanceFromIndex(params.victim_entindex))
            {
                ammo.Destroy();
                break;
            }
        }            
    }

    OnGameEvent_player_spawn = function(params)
    {
        local player = GetPlayerFromUserID(params.userid);
        AddThinkToEnt(player, "PlayerThink");
    }

    OnGameEvent_teamplay_win_panel = function(params)
    {
        SetData("winningTeam", params.winning_team);
        SetData("blueScore", params.blue_score);
        SetData("redScore", params.red_score);
        SetData("streetpassConvars", streetpassConvars);

        matchEnded = true;
        winnerTeam = params.winning_team;

        if(topAreaTrigger != null)
        {
            topAreaTrigger.AcceptInput("Disable", "", null, null);
            topProtected = false;
            FireScriptEvent("sp_top_protection_disabled", {});
        }

        //Sort players by team
        local players = [];
        for (local i = 1; i <= MaxPlayers; i++) {
            local player = PlayerInstanceFromIndex(i);
            if (IsPlayerValid(player) == false) { continue; }
            
            if(player.GetTeam() == params.winning_team)
            {
                players.insert(0, i);
            }else
            {
                players.push(i);
            }
        }

        //Print Player stats
        ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS] \x07FFFF00Stats: ");
        for (local i = 0; i < players.len(); i++)
        {
            local player = PlayerInstanceFromIndex(players[i]);

            local pName = NetProps.GetPropString(player, "m_szNetname");
            local scores = GetStat(players[i], STAT_SCORE);
            local assists = GetStat(players[i], STAT_ASSIST);
            local intercepts = GetStat(players[i], STAT_INTERCEPT);
            local steals = GetStat(players[i], STAT_STEAL);
            local swaps = GetStat(players[i], STAT_SWAP);

            local team = "FFFF00";
            if(player.GetTeam() == RED)
                team = "FF3F3F";
            else if(player.GetTeam() == BLUE)
                team = "99CCFF";

            ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, 
                "\x07"+team+pName+"\x07FFFF00 | Scores: "+scores+"\x07FFFF00 | Assists: "+assists+"\x07FFFF00 | Intercepts: "+intercepts+"\x07FFFF00 | Steals: "+steals+" | Swaps: "+swaps);
        }
        ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x07FFFF00Side swaps: "+sideSwaps);
    
        for (local i = 1; i <= MaxPlayers; i++)
        {
            local player = PlayerInstanceFromIndex(i)
            if (IsPlayerValid(player) == false) { continue; }

            if(player.GetTeam() != winnerTeam)
            {
                player.SetHealth(player.GetMaxHealth());
            }
        }
    }

    OnGameEvent_post_inventory_application = function (params) {

        local player = GetPlayerFromUserID(params.userid);
        local removed = false;
        local last_good_weapon = null;
        //ban stock weapons
        for (local i = 0; i < MAX_WEAPONS; i++)
        {
            local held_weapon = NetProps.GetPropEntityArray(player, "m_hMyWeapons", i);
            if(held_weapon == null)
            {
                continue;
            }

            local weapon_class = held_weapon.GetClassname();
            if(weapon_class == "tf_weapon_shotgun_soldier")
            {
                removed = true;
                ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS]\x07FF0a00 Shotgun is not allowed!");
                NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
            } else if(weapon_class == "tf_weapon_shotgun_pyro")
            {
                removed = true;
                ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS]\x07FF0a00 Shotgun is not allowed!");
                NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
            } else if(weapon_class == "tf_weapon_shotgun")
            {
                removed = true;
                ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS]\x07FF0a00 Shotgun is not allowed!");
                NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
            } else if(weapon_class == "tf_weapon_pipebomblauncher")
            {
                removed = true;
                ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS]\x07FF0a00 Stickybomb launcher is not allowed!");
                NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
            } else if(weapon_class == "tf_weapon_syringegun_medic")
            {
                removed = true;
                ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS]\x07FF0a00 Syringe gun is not allowed!");
                NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
            } else if(weapon_class == "tf_weapon_rocketlauncher_fireball")
            {
                local cooldown = GetSpCvar("sp_pyro_airblast_charge_rate");
                if(cooldown == null)
                {
                    cooldown = 0.8;
                }
                held_weapon.AddAttribute("item_meter_charge_rate", cooldown, 0);
            } else if(weapon_class == "tf_weapon_flamethrower")
            {
                removed = true;
                ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS]\x07FF0a00 Flamethrower is not allowed!");
                NetProps.SetPropEntityArray(player, "m_hMyWeapons", null, i);
            } else
            {
                if (last_good_weapon == null)
                {
                   last_good_weapon = held_weapon;
                }
            }
        }
    
        //set active weapon to a valid one after removal
        if(removed)
        {
            if (last_good_weapon != null)
            {
                player.Weapon_Switch(last_good_weapon);
            }
        }
    }

    // OnGameEvent_teamplay_round_start = function(params)
    //HACK(i guess): event above starts before spawn wich leads to incorect spawns but this is before so im gonna use this xd
    OnGameEvent_recalculate_holidays = function(params)
    {
        if(GetData("winningTeam") == BLUE)
        {
            //Reset Scores
            gamerules.AcceptInput("AddBlueTeamScore", "-"+GetData("blueScore"), null, null);
            gamerules.AcceptInput("AddRedTeamScore", "-"+GetData("redScore"), null, null);

            gamerules.AcceptInput("AddBlueTeamScore", ""+GetData("redScore"), null, null);
            gamerules.AcceptInput("AddRedTeamScore", ""+GetData("blueScore"), null, null);

            local blue = Convars.GetStr("mp_tournament_blueteamname");
            local red = Convars.GetStr("mp_tournament_redteamname");
            Convars.SetValue("mp_tournament_blueteamname", red);
            Convars.SetValue("mp_tournament_redteamname", blue);

            for (local i = 1; i <= MaxPlayers ; i++)
            {
                local player = PlayerInstanceFromIndex(i)
                if (IsPlayerValid(player) == false) { continue; }

                PlayerSwapTeam(player);
            }
        }

        if(topAreaTrigger != null)
        {
            topAreaTrigger.AcceptInput("Enable", "", null, null);
            topProtected = true;
            FireScriptEvent("sp_top_protection_enabled", {});
        }

        if(IsDedicatedServer())
        {
            SendToConsole("exec streetpass_vscripts.cfg");
        } else
        {
            SendToServerConsole("exec streetpass_vscripts.cfg");
        }
    }

    //redo vars on round restart
    OnGameEvent_teamplay_restart_round = function(params)
    {
        SetData("winningTeam", RED);
        SetData("blueScore", 0);
        SetData("redScore", 0);
    }

    OnScriptHook_OnTakeDamage = function(params)
    {
        local victim = params.const_entity;
        local attacker = params.attacker;

        if(params.weapon != null)
        {
            local weapon = params.weapon;
            if(weapon.GetClassname() == "tf_weapon_flaregun")
            {
                local splash_radius = GetSpCvar("sp_pyro_detonator_splash_radius");
                if(victim.GetClassname() == "passtime_ball" && victim.GetTeam() != 0)
                {
                    if(!splash_radius || (params.inflictor.GetOrigin() - victim.GetOrigin()).Length() > splash_radius)
                    {
                        params.damage = 0;
                        params.const_base_damage = 0;
                        params.early_out = true;
                        return;
                    }
                }

                local pushmult = GetSpCvar("sp_pyro_detonator_knockback_mult");
                if(pushmult == null)
                {
                    pushmult = 1.0;
                }
                params.damage *= pushmult;
            }
        }

        if(victim.GetClassname() == "passtime_ball" && IsPlayerValid(attacker))
        {
            jackTeam = 0;
            if(victim.GetTeam() != 0)
            {
                FireScriptEvent("sp_pass_splashed", {splasher = attacker.entindex(), old_ball = victim.GetTeam()});
            }
            return;
        }
        

        if(IsPlayerValid(attacker) && GetStat(attacker, STAT_BLOCKDAMAGETICK) == GetFrameCount())
        {
            params.damage = 0;
            params.early_out = true;
            return;
        }

        if(victim.GetName() == "block_caber")
        {
            if(IsPlayerValid(attacker))
            {
                SetStat(attacker, STAT_BLOCKDAMAGETICK, GetFrameCount());
            }
            return;
        }

        if(!IsPlayerValid(victim) || !GetSpCvar("sp_medic_replicates_blast_jump"))
        {
            return;
        }

        for(local i = 1; i <= MaxPlayers; i++)
        {
            local player = PlayerInstanceFromIndex(i);
            if(!IsPlayerValid(player) || player.entindex() == victim.entindex() || player.GetTeam() != victim.GetTeam())
            {
                continue;
            }

            if(player.GetPlayerClass() == Constants.ETFClass.TF_CLASS_MEDIC)
            {
                local victim_pos = victim.GetOrigin();
                local medic_pos = player.GetOrigin();

                if((medic_pos - victim_pos).Length() < packRange && !(player.GetFlags() & Constants.FPlayer.FL_ONGROUND) && player.GetActiveWeapon().GetClassname() == "tf_weapon_passtime_gun")
                {
                    
                    if(GetSpCvar("sp_medic_replicates_caber") && params.weapon != null && params.weapon.GetClassname() == "tf_weapon_stickbomb")
                    {
                        local demo_vel = victim.GetAbsVelocity();
                        local medic_vel = player.GetAbsVelocity();
                        medic_vel.z = 620;
                        player.SetAbsVelocity(medic_vel);
                    } else
                    {
                        local force = params.damage_force.Scale(1.0 / pow(2, 5));
                        player.SetAbsVelocity(player.GetAbsVelocity() + force);
                    }
                    break;
                }
            }
        }
    }

    OnGameEvent_pass_get = function(params)
    {
        local owner = PlayerInstanceFromIndex(params.owner);
        jackTeam = owner.GetTeam();
        
        if(owner.GetTeam() == defenseTeam)
        {
            if(topAreaTrigger != null && GetSpCvar("sp_top_protection_time") != 0)
            {
                topAreaTrigger.AcceptInput("Disable", "", null, null);
                topProtected = false;
                FireScriptEvent("sp_top_protection_disabled", {});
            }
        }
    }

    OnGameEvent_pass_free = function(params)
    {
        local owner = PlayerInstanceFromIndex(params.owner);
        jackOwner = params.owner;
        jackTeam = owner.GetTeam();
    }

    OnGameEvent_pass_score = function(params)
    {
        // scorer (short)
        // assister (short)
        // points (byte)
        if(GetSpCvar("sp_roundtimer_addtime"))
        {
            timer.AcceptInput("AddTime", GetSpCvar("sp_roundtimer_addtime").tostring(), self, self);
        }

        local scorer = PlayerInstanceFromIndex(params.scorer);
        local sName = NetProps.GetPropString(scorer, "m_szNetname");
        IncStat(params.scorer, STAT_SCORE);

        IncStat(scorer, STAT_KILLSTREAK, 2);
        instantRespawn = true;
        ballSpawned = false;
        if(topAreaTrigger != null && GetSpCvar("sp_top_protection_time") != 0)
        {
            topProtected = true;
            topAreaTrigger.AcceptInput("Enable", "", null, null);
            FireScriptEvent("sp_top_protection_enabled", {});
        }

        if(params.assister < 0)
        {
            ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS] \x01"+sName+"\x01 Scored!");
        }else
        {
            local assister = PlayerInstanceFromIndex(params.assister);
            local aName = NetProps.GetPropString(assister, "m_szNetname");
            IncStat(params.assister, STAT_ASSIST);

            IncStat(assister, STAT_KILLSTREAK, 1);

            ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS] \x01"+sName+"\x01 Scored! Assisted by "+aName);
        }
    }

    OnGameEvent_pass_pass_caught = function(params)
    {
        // Intercept check for catchers team
        // passer (short)
        // catcher (short)
        // dist (float)
        // duration (float)
        
        local passer = PlayerInstanceFromIndex(params.passer);
        local catcher = PlayerInstanceFromIndex(params.catcher);

        if(catcher.GetTeam() != passer.GetTeam())
        {
            local pName = NetProps.GetPropString(passer, "m_szNetname");
            local cName = NetProps.GetPropString(catcher, "m_szNetname");
            IncStat(params.catcher, STAT_INTERCEPT);
            FireScriptEvent("sp_pass_intercept", {victim = params.passer, intercepter = params.catcher});

            ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS] \x01"+cName+"\x01 Intercepted "+pName+"\x01 throw!");
        }
    }

    OnGameEvent_pass_ball_stolen = function(params)
    {
        // Melee steal
        // victim (short)
        // attacker (short)
        local victim = PlayerInstanceFromIndex(params.victim);
        local attacker = PlayerInstanceFromIndex(params.attacker);
        local vName = NetProps.GetPropString(victim, "m_szNetname");
        local aName = NetProps.GetPropString(attacker, "m_szNetname");
        
        if(victim.GetTeam() == attacker.GetTeam())
        {
            return;
        }

        IncStat(params.attacker, STAT_STEAL);

        ClientPrint(null, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS] \x01"+aName+"\x01 Stole the ball from "+vName+"\x01!");
    }

    OnGameEvent_teamplay_broadcast_audio = function(params) {
        if(params.sound == "Passtime.BallSpawn")
        {
            instantRespawn = false;
            FireScriptEvent("sp_pass_spawn", {});

            if(topAreaTrigger == null || GetSpCvar("sp_top_protection_time") == 0)
            {
                return;
            }

            ballSpawned = true;
            ballSpawnTime = Time();
            local numAttackers = 0;
            local numInside = 0;
            for(local i = 1; i <= MaxPlayers; i++)
            {
                local player = PlayerInstanceFromIndex(i);
                if(!IsPlayerValid(player))
                {
                    continue;
                }

                if(player.GetTeam() == attackerTeam)
                {
                    numAttackers++;
                }

                local pos = player.GetOrigin();
                local mins = topAreaTrigger.GetCenter() + topAreaTrigger.GetBoundingMins();
                local maxs = topAreaTrigger.GetCenter() + topAreaTrigger.GetBoundingMaxs();
                
                if(pos.z >= mins.z && pos.x >= mins.x && pos.y >= mins.y && pos.x <= maxs.x && pos.y <= maxs.y)
                {
                    if(player.GetTeam() == attackerTeam)
                    {
                        numInside++;
                    } else if(player.GetTeam() == defenseTeam)
                    {
                        player.ForceRespawn();
                        ClientPrint(player, Constants.EHudNotify.HUD_PRINTTALK, "\x07FF9100[StreetPASS]\x01 Top area is protected!");
                    }
                }
            }
            if(!numInside || numInside < numAttackers)
            {
                topAreaTrigger.AcceptInput("Disable", "", null, null);
                topProtected = false;
                FireScriptEvent("sp_top_protection_disabled", {});
            } else {
                topAreaTrigger.AcceptInput("Enable", "", null, null);
                topProtected = true;
                FireScriptEvent("sp_top_protection_enabled", {});
            }
        }
    }
}

function OnPostSpawn()
{
    local spawn = null;
    while(spawn = Entities.FindByClassname(spawn, "info_player_teamspawn"))
    {
        if(spawn.GetTeam() == BLUE)
        {
            blueSpawns.append(spawn);
        }
        else {
            redSpawns.append(spawn);
        }
    }
}

local EventsTable = getroottable()[EventsID]
foreach (name, callback in EventsTable) EventsTable[name] = callback.bindenv(this)
__CollectGameEventCallbacks(EventsTable)
