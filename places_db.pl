:- dynamic price/2.

price(teatr_syrena, 120).
price(biskupin, 10).
price(kino_wisla, 25).
price(niewidzialna_wystawa, 20).
price(spa_resort, 1000).
price(centrum_nauki_kopernik, 30).
price(domek_w_bieszczadach, 500).
price(camping_w_mielnie, 100).
price(zamek_krolewski, 20).
price(kampinowski_park_narodowy, 0).
price(agroturystyka, 600).
price(zwiedzanie_starego_miasta_z_przewodnikiem, 5).

:- dynamic visit_time/2.

visit_time(teatr_syrena, 180).
visit_time(biskupin, 720).
visit_time(kino_wisla, 120).
visit_time(niewidzialna_wystawa, 150).
visit_time(spa_resort, 4320).
visit_time(centrum_nauki_kopernik, 360).
visit_time(domek_w_bieszczadach, 10080).
visit_time(camping_w_mielnie, 8640).
visit_time(zamek_krolewski, 200).
visit_time(kampinowski_park_narodowy, 500).
visit_time(agroturystyka, 7000).
visit_time(zwiedzanie_starego_miasta_z_przewodnikiem, 90).

:- dynamic climate/2.

climate(teatr_syrena, kontynentalny).
climate(biskupin, kontynentalny).
climate(kino_wisla, kontynentalny).
climate(niewidzialna_wystawa, kontynentalny).
climate(spa_resort, gorski).
climate(centrum_nauki_kopernik, kontynentalny).
climate(domek_w_bieszczadach, gorski).
climate(camping_w_mielnie, morski).
climate(zamek_krolewski, kontynentalny).
climate(kampinowski_park_narodowy, kontynentalny).
climate(agroturystyka, kontynentalny).
climate(zwiedzanie_starego_miasta_z_przewodnikiem, kontynentalny).

:- dynamic activities_for_children/2.

activities_for_children(teatr_syrena, 1).
activities_for_children(biskupin, 23).
activities_for_children(kino_wisla, 1).
activities_for_children(niewidzialna_wystawa, 0).
activities_for_children(spa_resort, 0).
activities_for_children(centrum_nauki_kopernik, 68).
activities_for_children(domek_w_bieszczadach, 3).
activities_for_children(camping_w_mielnie, 5).
activities_for_children(zamek_krolewski, 1).
activities_for_children(kampinowski_park_narodowy, 15).
activities_for_children(agroturystyka, 36).
activities_for_children(zwiedzanie_starego_miasta_z_przewodnikiem, 1).

:- dynamic tourists_visiting/2.

tourists_visiting(teatr_syrena, 300).
tourists_visiting(biskupin, 2465).
tourists_visiting(kino_wisla, 150).
tourists_visiting(niewidzialna_wystawa, 30).
tourists_visiting(spa_resort, 75).
tourists_visiting(centrum_nauki_kopernik, 1350).
tourists_visiting(domek_w_bieszczadach, 5).
tourists_visiting(camping_w_mielnie, 25).
tourists_visiting(zamek_krolewski, 400).
tourists_visiting(kampinowski_park_narodowy, 800).
tourists_visiting(agroturystyka, 8).
tourists_visiting(zwiedzanie_starego_miasta_z_przewodnikiem, 65).

:- dynamic age_limit/2.

age_limit(teatr_syrena, nastolatkowie).
age_limit(biskupin, brak).
age_limit(kino_wisla, z_rodzicem).
age_limit(niewidzialna_wystawa, nastolatkowie).
age_limit(spa_resort, dorosli).
age_limit(centrum_nauki_kopernik, brak).
age_limit(domek_w_bieszczadach, nastolatkowie).
age_limit(camping_w_mielnie, z_rodzicem).
age_limit(zamek_krolewski, brak).
age_limit(kampinowski_park_narodowy, brak).
age_limit(agroturystyka, z_rodzicem).
age_limit(zwiedzanie_starego_miasta_z_przewodnikiem, brak).

:- dynamic has_meal/1.

has_meal(biskupin).
has_meal(spa_resort).
has_meal(centrum_nauki_kopernik).
has_meal(domek_w_bieszczadach).
has_meal(camping_w_mielnie).
has_meal(agroturystyka).

:- dynamic has_rooms_too_sleep/1.

has_rooms_too_sleep(spa_resort).
has_rooms_too_sleep(domek_w_bieszczadach).
has_rooms_too_sleep(camping_w_mielnie).
has_rooms_too_sleep(agroturystyka).

:- dynamic has_toilet/1.

has_toilet(teatr_syrena).
has_toilet(kino_wisla).
has_toilet(spa_resort).
has_toilet(centrum_nauki_kopernik).
has_toilet(domek_w_bieszczadach).
has_toilet(zamek_krolewski).

:- dynamic has_parking/1.

has_parking(biskupin).
has_parking(spa_resort).
has_parking(centrum_nauki_kopernik).
has_parking(domek_w_bieszczadach).
has_parking(camping_w_mielnie).
has_parking(agroturystyka).
has_parking(kampinowski_park_narodowy).

:- dynamic adjusted_for_disabled/1.

adjusted_for_disabled(teatr_syrena).
adjusted_for_disabled(biskupin).
adjusted_for_disabled(kino_wisla).
adjusted_for_disabled(spa_resort).
adjusted_for_disabled(centrum_nauki_kopernik).
adjusted_for_disabled(zamek_krolewski).
adjusted_for_disabled(kampinowski_park_narodowy).
