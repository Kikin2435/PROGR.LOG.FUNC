:- use_module(library(pce)).

mostrar(V,D,M):- new(I, image(V)),
        new(B, bitmap(I)),
        new(F2, figure),
        send(F2, display, B),
        new(D1, device),
        send(D1, display, F2),
        send(D, display, D1),
        send(D1,below(M)).

hierba_botiquin(anis).
hierba_botiquin(estrella).
hierba_botiquin(menta).
hierba_botiquin(arnica).
hierba_botiquin(savila).
hierba_botiquin(tila).
hierba_botiquin(eucalipto).
hierba_botiquin(yerbabuena).
hierba_botiquin(manzanilla).
hierba_botiquin(cola_de_caballo).
hierba_botiquin(romero).
hierba_botiquin(toronjil).
hierba_botiquin(sanguinaria).
hierba_botiquin(linaza).
hierba_botiquin(hamamelis).
hierba_botiquin(zarzaparrilla).
hierba_botiquin(boloo).
hierba_botiquin(diente_de_leon).
hierba_botiquin(azahar).
hierba_botiquin(malva).
hierba_botiquin(marrubio).
hierba_botiquin(rosal).

hierba_medicinal(abrojo, [afecciones_del_pecho, higado, circulacion, inflamacion_del_ojo, infeccion_urinaria, cistitis, infeccion_rinon]).
hierba_medicinal(acacia, [dolor_de_garganta, tos, bronquitis]).
hierba_medicinal(acanto, [antivenenoso, emoliente]).
hierba_medicinal(aceitilla, [cansancio_intelectual, depresion_nerviosa]).
hierba_medicinal(achicoria, [diuretico, depurativo]).
hierba_medicinal(sanguinaria, [depurativa,diuretica]).
hierba_medicinal(sensitiva, [insomnio, cansancio]).
hierba_medicinal(simonillo, [icteria,catarro_vias_biliares,colico_hepatico]).
hierba_medicinal(tamarindo, [laxante,diuretico,refrescante]).
hierba_medicinal(amapola_amarilla,[hipnotica,sedante,antidiarreica]).
hierba_medicinal(anis, [indigestion,gases,nauseas]).
hierba_medicinal(amate, [depuratico,estimulante]).
hierba_medicinal(ruda, [abortiva, alivia_jaquecas, regula_menstruacion]).
hierba_medicinal(ruibaro, [contra_disenteria, alivia_flatulencias, diuretico, estimulante_digestivo]).
hierba_medicinal(salvia, [limpia_dientes, purifica_aliento]).
hierba_medicinal(sen, [alivia_estrenimiento, antianemico]).
hierba_medicinal(marihuana, [cancer, glaucoma, males_ojos, artritis, reumatismo]).
hierba_medicinal(matarique, [diabetes, reumatismo, rinones_adoloridos]).
hierba_medicinal(digitaria, [miocarditis, astenia, epilepsia]).
hierba_medicinal(cardo_santo, [nubes_ojos]).
hierba_medicinal(brionia, [lombrices]).
hierba_medicinal(canela, [anemia]).
hierba_medicinal(cedron, [tos]).
hierba_medicinal(doradilla, [nefritis]).
hierba_medicinal(epazote, [lombrices]).
hierba_medicinal(enebro, [leucorrea]).
hierba_medicinal(mangle, [diarrea, inflamaciones, heridas]).
hierba_medicinal(manzanilla, [insomnio, agruras, higado_colitis, jaqueca, menstruacion, neuralgias, irritacion_ojo]).
hierba_medicinal(marrubio, [asma, caida_cabello, obesidad, tos, reumatismo, vomitos]).
hierba_medicinal(mastuerzo, [ciatica, tuberculosis]).
hierba_medicinal(menta, [insomnio, lactancia, nauseas, neuralgias, vomitos, sarna]).
hierba_medicinal(aconito, [neuralgia, fiebre, reumatismo]).
hierba_medicinal(adormidera, [insomnio, dolor, ansiedad]).
hierba_medicinal(ahuehuete, [enfermedades_respiratorias, infecciones_piel]).
hierba_medicinal(ajo, [reumas, sarna, tina, callos, lombrices]).
hierba_medicinal(albahaca, [alopecia]).
hierba_medicinal(alcachofa, [diabetes, anemia]).
hierba_medicinal(aguacate, [estrenimiento, problemas_digestivos, tos]).
hierba_medicinal(ajenjo, [parasitos_intestinales, problemas_digestivos, debilidad_general]).
hierba_medicinal(alcanfor, [gota, piquetes_mosco, tifoidea, artritis, arteriosclerosis]).
hierba_medicinal(anacahuite, [bronquitis, tos, pulmones, resfriado]).
hierba_medicinal(barbasco, [verrugas, tina, sarna, anticonceptivo]).
hierba_medicinal(amapola_amarilla, [diarrea, insomnio, ansiedad_leve]).
hierba_medicinal(amate, [reumatismo, diviesos, solitaria, inflamacion, infecciones_leves]).
hierba_medicinal(anis, [colitis_leve, indigestion, flatulencias, colicos, tos, bronquitis]).
hierba_medicinal(arnica, [golpes, torceduras, moretones]).
hierba_medicinal(belladona, [espasmos, colicos, dolores_menstruales, asma, parkinson]).
hierba_medicinal(berro, [bronquitis]).
hierba_medicinal(boldo, [problemas_hepaticos]).
hierba_medicinal(borraja, [fiebre]).
hierba_medicinal(bugambilia, [tos]).
hierba_medicinal(cempasuchil, [parasitos_intestinales, tumores]).
hierba_medicinal(chaparro_amargoso, [disenteria_amebiana, diarrea, flujo, hemorragias_internas]).
hierba_medicinal(chicalote, [tos, asma, tosferina, epilepsia, artritis, insomnio, ansiedad, colicos_hepaticos, colicos_renales, colicos_intestinales, carnosidad_ojos]).
hierba_medicinal(chile, [asma, reumatismo]).
hierba_medicinal(chichigua, [dermatitis, inflamacion, resfriado]).
hierba_medicinal(cocolmeca, [reumatismo, acne, anemia]).
hierba_medicinal(cola_de_caballo, [retencion_liquidos, calculos_renales]).
hierba_medicinal(colchino, [dolor_estomacal, parasitos, problemas_hepaticos]).
hierba_medicinal(colpachi, [sarampion, afecciones_de_la_piel, fiebre]).
hierba_medicinal(cuajiote, [anasarca, estrenimiento_cronico]).
hierba_medicinal(cuasia, [diabetes, artritis, reumatismo, dolor_corporal, migrana, dolor_de_estomago]).
hierba_medicinal(diente_de_leon, [anemia, acumulacion_de_toxinas]).
hierba_medicinal(cilantro, [problemas_digestivos, ansiedad]).
hierba_medicinal(comino, [indigestion, flatulencias, colico_menstrual]).
hierba_medicinal(cuachalalate, [ulceras, infecciones_gastrointestinales, problemas_bucales]).
hierba_medicinal(damiana, [hipersexualidad, alcoholismo, diabetes, nefritis, orquitis, males_de_la_vejiga]).
hierba_medicinal(digitaria, [miocarditis, astenia, epilepsia]).
hierba_medicinal(doradilla, [nefritis]).
hierba_medicinal(enebro, [leucorrea]).
hierba_medicinal(fenogreco, [diabetes, colesterol]).
hierba_medicinal(geranio, [estres, insomnio]).
hierba_medicinal(girasol, [presion_alta, fiebre]).
hierba_medicinal(gingseng, [diabetes]).
hierba_medicinal(grama, [cistitis]).
hierba_medicinal(guaco, [alergia, vitiligo, asma]).
hierba_medicinal(gordolobo, [tos, bronquitis]).
hierba_medicinal(granado, [disenteria, parasitos_intestinales]).
hierba_medicinal(guazuma, [disenteria, diarrea, inflamacion_intestinal]).
hierba_medicinal(guayacan, [tos, tuberculosis, sifilis, reumatismo]).
hierba_medicinal(hamamelis, [hemorroides, varices, retencion_orina]).
hierba_medicinal(helenio, [bronquitis, tos_ferina, retencion_orina]).
hierba_medicinal(madresilva, [gripa, tos, infecciones_garganta]).
hierba_medicinal(maguey, [llagas, infecciones_piel, fiebre]).
hierba_medicinal(hierba_del_pollo, [hemorragia, problemas_renales]).
hierba_medicinal(jalapa, [disenteria, estrenimiento, indigestion, apoplejia, congestion_cerebral]).
hierba_medicinal(ipecacuana, [tos]).
hierba_medicinal(jazmin_amarillo, [dolores_de_cabeza, reuma, espasmos, asma_bronquial, menstruacion_dolorosa]).
hierba_medicinal(linaza, [estrenimiento, colitis, males_estomacales, bronquitis, hemorroides, heridas, abscesos]).
hierba_medicinal(llanten, [conjuntivitis, infeccion_ojos, ulceras_boca, pequenas_infecciones, disenteria, enterocolitis]).
hierba_medicinal(hinojo, [gases, flatulencias, obstruccion_mucosa_pecho]).
hierba_medicinal(maiz, [problemas_renales, hipertension, problemas_digestivos, inflamacion, diabetes]).
hierba_medicinal(malva, [irritacion_garganta, estrenimiento, problemas_piel, ulceras, inflamacion_bucal]).
hierba_medicinal(malvavisco, [bronquitis, gastritis, eczema, faringitis, ciatica]).
hierba_medicinal(mangle, [leishmaniasis, diarrea_cronica, hemorragias, infecciones_cutaneas, hepatitis]).
hierba_medicinal(marrubio, [asma, caida_cabello, obesidad, tos, reumatismo, vomitos]).
hierba_medicinal(marihuana, [cancer, glaucoma, males_ojos, artritis, reumatismo]).
hierba_medicinal(mastuerzo, [ciatica, tuberculosis]).
hierba_medicinal(matarique, [diabetes, reumatismo, rinones_adoloridos]).
hierba_medicinal(palo_de_flor, [fiebre, dolor_de_cabeza]).
hierba_medicinal(pinguica, [infeccion_urinaria, rinones]).
hierba_medicinal(ruibarbo, [estrenimiento, digestion, higado]).
hierba_medicinal(sen, [estrenimiento]).
hierba_medicinal(sanguinaria, [problemas_respiratorios, dolor_garganta, tos]).
hierba_medicinal(sensativa, [ansiedad, insomnio]).
hierba_medicinal(simonillo, [problemas_estomacales, parasitos_intestinales]).
hierba_medicinal(tabachin, [tos, bronquitis]).
hierba_medicinal(taray, [problemas_renales, inflamacion_ojos]).
hierba_medicinal(regaliz, [tos, dolor_garganta, problemas_digestivos]).
hierba_medicinal(manzanilla, [ansiedad, insomnio, indigestion, conjuntivitis, eczema, colico_menstrual]).
hierba_medicinal(menta, [insomnio, lactancia, nauseas, neuralgias, vomitos, sarna]).
hierba_medicinal(oregano, [problemas_digestivos, resfriado]).
hierba_medicinal(pasiflora, [insomnio, ansiedad]).
hierba_medicinal(pericon, [colicos, indigestion]).
hierba_medicinal(ruda, [menstruacion, colicos, nerviosismo]).
hierba_medicinal(salvia, [dolor_garganta, digestion, sudoracion_excesiva]).
hierba_medicinal(tamarindo, [estrenimiento, fiebre]).
hierba_medicinal(retama, [hipertension, reumatismo]).
hierba_medicinal(ricino, [estrenimiento, inflamacion]).
hierba_medicinal(rosal, [estres, problemas_digestivos]).
hierba_medicinal(nogal, [anemia, escrofulosis, herpes, reumatismo]).
hierba_medicinal(nuez_vomica, [fiebres_malignas, bronquitis, reumas, lombrices_intestinales, tos_ferina]).
hierba_medicinal(ocote, [problemas_respiratorios, dolor_muscular]).
hierba_medicinal(ortiga, [anemia, artritis, reumatismo, problemas_cutaneos, caida_cabello]).
hierba_medicinal(prodigiosa, [disenteria, cirrosis_hepatica, ictericia]).
hierba_medicinal(pirul, [gonorrea]).
hierba_medicinal(pulsatilla, [herpes, tos_ferina, enfermedades_venereas, jaquecas_neuronales]).
hierba_medicinal(quebracho, [inflamaciones_intestinales, flujo, afecciones_del_rinon]).
hierba_medicinal(quina, [tos_ferina, asma, tetano, epilepsia, eclampsia]).
hierba_medicinal(toloache, [dolor_muscular, asma, insomnio]).
hierba_medicinal(tronadora, [diabetes, fiebre, problemas_digestivos]).
hierba_medicinal(tripa_de_judas, [tos, gripe, dolor_estomacal]).
hierba_medicinal(uva, [estrenimiento, hipertension, anemia]).
hierba_medicinal(nopal, [diabetes, inflamacion_vejiga, heridas, hinchazones]).
hierba_medicinal(romero, [fatiga, problemas_digestivos]).
hierba_medicinal(maiz, [problemas_renales, hipertension, problemas_digestivos, inflamacion, diabetes, infeccion_urinaria, retencion_liquidos]).
hierba_medicinal(tila, [insomnio, ansiedad, hipertension]).

propiedades_planta(sanguinaria, [depurativa, diuretica]).
propiedades_planta(sensitiva, [somnifera, relajante]).
propiedades_planta(simonillo, [ictericia, catarro_vias_biliares, colico_hepatico]).
propiedades_planta(tamarindo, [laxante, diuretico, refrescante]).
propiedades_planta(test, [laxante, diuretico, refrescante]).
propiedades_planta(amapola_amarilla, [hipnotica,sedante,antidiarreica]).
propiedades_planta(amate, [depuratico,estimulante]).
propiedades_planta(anis, [desinfectante,autiflatulento]).
propiedades_planta(ruda, [abortiva, alivia_jaquecas, regula_menstruacion]).
propiedades_planta(ruibaro, [tapa, desinflamante, diuretico, estimulante]).
propiedades_planta(salvia, [limpia_dientes, purifica_aliento]).
propiedades_planta(sen, [alivia_estrenimiento, antianemico]).
propiedades_planta(marihuana, [analgesica, antiinflamatoria]).
propiedades_planta(matarique, [desinflamatoria, hipoglucemiante]).
propiedades_planta(digitaria, [cardiotonica]).
propiedades_planta(cardo_santo, [oftalmica]).
propiedades_planta(brionia, [purgante, antiparasitaria]).
propiedades_planta(canela, [estimulante, antimicrobiana]).
propiedades_planta(cedron, [expectorante, calmante]).
propiedades_planta(doradilla, [diuretica, desinflamatoria]).
propiedades_planta(epazote, [antiparasitaria, carminativa]).
propiedades_planta(enebro, [antiseptica, diuretica]).
propiedades_planta(mangle, [astringente, antiinflamatoria]).
propiedades_planta(manzanilla, [calmante, antiinflamatoria, digestiva]).
propiedades_planta(marrubio, [expectorante, digestiva]).
propiedades_planta(mastuerzo, [antiinflamatoria, expectorante]).
propiedades_planta(menta, [digestiva, calmante, refrescante]).
propiedades_planta(aconito, [analgesico, antipiretico]).
propiedades_planta(adormidera, [sedante, analgesico]).
propiedades_planta(ahuehuete, [antiseptico, astringente]).
propiedades_planta(ajo, [antiinflamatorio, vermifugo, febrifugo, diuretico, expectorante, antiparasitario]).
propiedades_planta(albahaca, [tonico_capilar, diuretica, carminativa, emenagoga]).
propiedades_planta(alcachofa, [descongestionante, desinflamatorio]).
propiedades_planta(abrojo, [diuretico, antiinflamatorio]).
propiedades_planta(aguacate, [laxante, expectorante]).
propiedades_planta(ajenjo, [antiparasitario, digestivo]).
propiedades_planta(alcanfor, [analgesico, antiseptico]).
propiedades_planta(anacahuite, [expectorante, balsamico]).
propiedades_planta(barbasco, [toxica, irritante, pesticida]).
propiedades_planta(amapola_amarilla, [sedante_suave, antiespasmodico]).
propiedades_planta(amate, [antiparasitario, antiinflamatorio]).
propiedades_planta(anis, [carminativo, expectorante]).
propiedades_planta(arnica, [antiinflamatoria, cicatrizante, analgesica]).
propiedades_planta(belladona, [antiespasmodica, analgesica, sedante, toxica]).
propiedades_planta(berro, [expectorante, depurativa]).
propiedades_planta(boldo, [hepatoprotectora, colagoga]).
propiedades_planta(borraja, [sudorifica, antiinflamatoria]).
propiedades_planta(bugambilia, [expectorante, antitusiva]).
propiedades_planta(brionia, [purgante, antiparasitaria]).
propiedades_planta(canela, [estimulante, antimicrobiana]).
propiedades_planta(cedron, [expectorante, calmante]).
propiedades_planta(cardo_santo, [oftalmica]).
propiedades_planta(cempasuchil, [antiparasitario, tonico]).
propiedades_planta(chaparro_amargoso, [antiseptico]).
propiedades_planta(chicalote, [antiespasmodico, hipnotico, sedante]).
propiedades_planta(chile, [antiinflamatorio, rubefaciente]).
propiedades_planta(chichigua, [antiinflamatorio, antiseptico]).
propiedades_planta(cocolmeca, [depurativo, antiinflamatorio]).
propiedades_planta(cola_de_caballo, [diuretico, remineralizante]).
propiedades_planta(colchino, [analgesico, antiparasitario, hepatoprotector]).
propiedades_planta(colpachi, [antipiretico, antiseptico, dermatologico]).
propiedades_planta(cuajiote, [laxante]).
propiedades_planta(cuasia, [analgesico, antiinflamatorio, hipoglucemiante]).
propiedades_planta(diente_de_leon, [aperitivo, depurativo, laxante, colagogo, diuretico, alimenticio]).
propiedades_planta(cilantro, [carminativo, relajante]).
propiedades_planta(comino, [carminativo, antiespasmodico, digestivo]).
propiedades_planta(cuachalalate, [cicatrizante, astringente, antiinflamatorio]).
propiedades_planta(damiana, [relajante, diuretico, antiinflamatorio]).
propiedades_planta(digitaria, [cardiotonica]).
propiedades_planta(doradilla, [diuretica, desinflamatoria]).
propiedades_planta(epazote, [antiparasitaria, carminativa]).
propiedades_planta(enebro, [antiseptica, diuretica]).
propiedades_planta(fenogreco, [hipoglucemiante, hipolipemiante]).
propiedades_planta(geranio, [relajante, ansiolitico]).
propiedades_planta(girasol, [diuretico, antiinflamatorio]).
propiedades_planta(gingseng, [estimulante, adaptogeno]).
propiedades_planta(grama, [diuretico, depurativo]).
propiedades_planta(guaco, [antiinflamatorio, broncodilatador]).
propiedades_planta(estafiate, [digestivo, antiparasitario]).
propiedades_planta(eucalipto, [expectorante, antiseptico]).
propiedades_planta(genciana, [estimulante, tonico]).
propiedades_planta(gordolobo, [expectorante, antiinflamatorio]).
propiedades_planta(guazuma, [antidiarreico, antiinflamatorio]).
propiedades_planta(guayacan, [expectorante, antiesifilitico]).
propiedades_planta(hamamelis, [venotonico, diuretico]).
propiedades_planta(helenio, [expectorante, diuretico]).
propiedades_planta(jengibre, [estimulante, digestiva]).
propiedades_planta(linaza, [emoliente, laxante]).
propiedades_planta(llanten, [antiinflamatoria, astringente]).
propiedades_planta(madresilva, [expectorante, antibacteriana]).
propiedades_planta(maguey, [cicatrizante, antimicrobiana, antipiretica]).
propiedades_planta(hierba_del_pollo, [hemostatico, cicatrizante, diuretico]).
propiedades_planta(hinojo, [digestivo]).
propiedades_planta(jalapa, [purgante]).
propiedades_planta(ipecacuana, [expectorante]).
propiedades_planta(jazmin_amarillo, [analgesico, antiespasmodico]).
propiedades_planta(maiz, [diuretico, antiinflamatorio, regulador_glucemia]).
propiedades_planta(malva, [emoliente, antiinflamatorio, laxante_suave, cicatrizante]).
propiedades_planta(malvavisco, [demulcente, antitusivo, antiacido, emoliente_intenso]).
propiedades_planta(mangle, [astringente_intenso, antiviral, hemostatico, cicatrizante, hepatoprotector]).
propiedades_planta(manzanilla, [sedante_suave, antiespasmodico, antiinflamatorio, carminativo, emenagogo]).
propiedades_planta(marrubio, [expectorante, digestiva]).
propiedades_planta(marihuana, [analgesica, antiinflamatoria]).
propiedades_planta(mastuerzo, [antiinflamatoria, expectorante]).
propiedades_planta(matarique, [desinflamatoria, hipoglucemiante]).
propiedades_planta(palo_de_flor, [febrifugo, analgesico]).
propiedades_planta(pinguica, [diuretico, depurativo]).
propiedades_planta(ruibarbo, [laxante, digestiva, tonica]).
propiedades_planta(sen, [laxante]).
propiedades_planta(sanguinaria, [expectorante, estimulante, antiseptica]).
propiedades_planta(sensativa, [sedante, relajante]).
propiedades_planta(simonillo, [digestivo, antiparasitario]).
propiedades_planta(tabachin, [expectorante, antitusivo]).
propiedades_planta(taray, [diuretico, antiinflamatorio]).
propiedades_planta(regaliz, [expectorante, digestivo]).
propiedades_planta(menta, [digestiva, calmante, refrescante]).
propiedades_planta(oregano, [antiespasmodico, expectorante]).
propiedades_planta(pasiflora, [sedante, ansiolitico]).
propiedades_planta(pericon, [digestivo, carminativo]).
propiedades_planta(ruda, [calmante, emenagoga, antiespasmodica]).
propiedades_planta(salvia, [astringente, digestiva, antiseptica]).
propiedades_planta(retama, [diuretico, hipotensor]).
propiedades_planta(ricino, [laxante, antiinflamatorio]).
propiedades_planta(rosal, [relajante, digestivo]).
propiedades_planta(nopal, [laxante, vermifugo]).
propiedades_planta(nogal, [tonico, antireumatico, nutritivo]).
propiedades_planta(nuez_vomica, [emetico, febrifugo]).
propiedades_planta(ocote, [expectorante, analgesico]).
propiedades_planta(ortiga, [diuretico, depurativo, antiinflamatorio]).
propiedades_planta(prodigiosa, [hepatoprotector, desintoxicante]).
propiedades_planta(pirul, [repelente, purgante]).
propiedades_planta(pulsatilla, [calmante, antiespasmodico]).
propiedades_planta(quebracho, [antiasmatico, descongestivo]).
propiedades_planta(quina, [antiasmatico, antimicrobiano]).
propiedades_planta(toloache, [analgesico, broncodilatador]).
propiedades_planta(tronadora, [hipoglucemiante, antipiretico]).
propiedades_planta(tripa_de_judas, [expectorante, antiespasmodico]).
propiedades_planta(uva, [antioxidante, laxante]).
propiedades_planta(romero, [estimulante, digestivo]).
propiedades_planta(tila, [calmante, sedante]).
propiedades_planta(maiz, [diuretico, antiinflamatorio, regulador_glucemia, depurativo]).

elementos_planta(tamarindo, [vitamina_A, vitamina_B, vitamina_C, acido_Acetico]).
elementos_planta(abrojo, [taninos, saponinas, flavonoides]).
elementos_planta(acacia, [goma, mucilago, taninos]).
elementos_planta(amapola_amarilla, [grasas, calorias, carbohidratos, fibras]).
elementos_planta(amate, [antihelmintico, enzima_proteolitica_ficina]).
elementos_planta(anis,[trans_anetol, anetol, albuminas]).
elementos_planta(ruda, [vitamina_C]).
elementos_planta(ruibaro, [aloe_emodina, diantronicos, senosidos]).
elementos_planta(salvia, [monoterpenos, msesquiterpenos]).
elementos_planta(sen, [estipulas]).

preparacion(cocimiento, 'hervir 40 minutos, reposar 5 minutos').
preparacion(infusion, 'pasar agua caliente por las hierbas').
preparacion(maceracion, 'moler y agregar agua hasta que saque sustancias').
preparacion(jarabe, 'hervir planta en agua por 10 minutos, agregar azucar y caramelizar 10 minutos, reposar').
preparacion(tintura, 'se usa alcohol para sacar los nutrientes').
preparacion(jugo, 'exprimir plantas con un trapo').
preparacion(horchata, 'moler y agregar agua de poco a poco hasta mazilla').
preparacion(desconocido, 'no se encuentra en el yerberito').
preparacion(te, 'Te por 2 o 3 veces por dia').
preparacion(mermelada, 'muela, hierba, agrege agua, hasta conseguir una pasta agregue asucar y listo!').
preparacion(masticado, 'Se mastica para limpiar el aliento').
preparacion(machacar, 'Se machaca la raiz para preparar un te').
preparacion(con_miel, 'Mezclar con miel para consumo').
preparacion(molido_con_leche, 'Moler y mezclar con leche').
preparacion(jugo, 'Exprimir para obtener jugo').
preparacion(cocer, 'Cocer en agua hirviendo').
preparacion(aceite, 'Preparar como aceite para aplicación externa').
preparacion(tintura, 'Usar alcohol para extraer nutrientes').
preparacion(infusion, 'Pasar agua caliente por las hierbas').
preparacion(cocimiento, 'Hervir durante 40 minutos y reposar 5 minutos').
preparacion(macerado, 'Macerar en agua o alcohol').
preparacion(decoccion, 'Hervir la corteza para obtener una decocción').
preparacion(macera, 'Macerar en alcohol o agua para extraer propiedades').
preparacion(horchata, 'Moler y agregar agua poco a poco hasta obtener mazilla').
preparacion(decoccion, 'Hervir la planta para obtener una decoccion').
preparacion(compresas_calientes, 'Preparar compresas calientes con cocimiento').
preparacion(infusion_de_hojas, 'Infusion de hojas en agua caliente').
preparacion(infusion_de_semillas, 'Infusion de semillas en agua caliente').
preparacion(decoccion_de_corteza, 'Decoccion de corteza hervida').
preparacion(infusion_o_decoccion_de_corteza, 'Infusion o decoccion de corteza en agua').
preparacion(vaporizacion, 'Inhalar vapor de la planta hervida').
preparacion(maceracion, 'Macerar la planta en agua o alcohol').
preparacion(infusion_cada_3_horas_o_cataplasma, 'Infusion cada 3 horas o aplicar como cataplasma').
preparacion(infusion_de_hojas, 'Infusion de hojas en agua caliente').
preparacion(infusion_de_flores_o_corteza, 'Infusion de flores o corteza en agua caliente').
preparacion(infusion_de_raiz, 'Infusion de raiz en agua caliente').
preparacion(tintura, 'Preparacion de tintura con alcohol').
preparacion(pan, 'Preparacion en forma de pan').
preparacion(remedio_efervescente, 'Preparacion como remedio efervescente').
preparacion(leche_hervida, 'Hervir hojas en leche').
preparacion(consumo_directo, 'Consumo directo de la planta').
preparacion(machacar, 'Machacar la planta para uso externo o infusion').
preparacion(infusion_raiz, 'Infusion de raiz en agua caliente').
preparacion(jarabe, 'Preparacion de jarabe con extracto de planta').
preparacion(cataplasma_hojas, 'Cataplasma de hojas frescas o hervidas').
preparacion(decoccion_corteza, 'Decoccion de corteza hervida').
preparacion(polvo_corteza, 'Polvo de corteza seca para aplicacion').
preparacion(masticar_propagulos, 'Masticar propagulos directamente').
preparacion(infusion_flores, 'Infusion de flores en agua caliente').
preparacion(aceite_esencial, 'Extraccion de aceite esencial para uso externo').
preparacion(compresas, 'Compresas con infusion de planta').
preparacion(tintura_30gotas_agua, 'Tintura diluida en agua, 30 gotas').
preparacion(gargaras, 'Preparacion para gargaras con infusion').
preparacion(te_flores, 'Te preparado con flores secas').
preparacion(infusion_vainas, 'Infusion de vainas en agua caliente').
preparacion(pulpa, 'Uso directo de pulpa de la planta').
preparacion(pencas, 'Uso de pencas frescas para consumo o cataplasma').
preparacion(hojas_y_corteza_en_te_o_infusion, 'Infusion o te de hojas y corteza').
preparacion(te_espanol, 'Preparacion de te al estilo espanol').
preparacion(hojas_te, 'Te preparado con hojas').
preparacion(resina_mezclada, 'Mezcla de resina para aplicacion externa').
preparacion(hojas_frescas_o_secas, 'Uso de hojas frescas o secas para infusion o cataplasma').
preparacion(jugo_o_consumo_directo, 'Jugo extraido o consumo directo de la planta').

planta_preparacion(abrojo, cocimiento).
planta_preparacion(acacia, desconocido).
planta_preparacion(acanto, horchata).
planta_preparacion(aceitilla, jugo).
planta_preparacion(achicoria, cocimiento).
planta_preparacion(sanguinaria, cocimiento).
planta_preparacion(sensitiva, cocimiento).
planta_preparacion(simonillo, cocimiento).
planta_preparacion(tamarindo, jugo).
planta_preparacion(amapola_amarilla, cocimiento).
planta_preparacion(amate, desconocido).
planta_preparacion(anis, cocimiento).
planta_preparacion(ruda, te).
planta_preparacion(ruibaro, mermelada).
planta_preparacion(salvia, masticado).
planta_preparacion(sen, machacar).
planta_preparacion(marihuana, maceracion).
planta_preparacion(matarique, cocimiento).
planta_preparacion(digitaria, infusion).
planta_preparacion(cardo_santo, cocimiento).
planta_preparacion(brionia, cocimiento).
planta_preparacion(canela, infusion).
planta_preparacion(cedron, infusion).
planta_preparacion(doradilla, cocimiento).
planta_preparacion(epazote, cocimiento).
planta_preparacion(enebro, infusion).
planta_preparacion(mangle, cocimiento).
planta_preparacion(manzanilla, infusion).
planta_preparacion(marrubio, infusion).
planta_preparacion(mastuerzo, cocimiento).
planta_preparacion(menta, infusion).
planta_preparacion(aconito, tintura).
planta_preparacion(adormidera, infusion).
planta_preparacion(ahuehuete, decoccion).
planta_preparacion(ajo, con_miel).
planta_preparacion(ajo, machacar).
planta_preparacion(ajo, molido_con_leche).
planta_preparacion(albahaca, jugo).
planta_preparacion(alcachofa, cocer).
planta_preparacion(abrojo, cocimiento).
planta_preparacion(aguacate, infusion).
planta_preparacion(ajenjo, infusion).
planta_preparacion(alcanfor, aceite).
planta_preparacion(anacahuite, cocimiento).
planta_preparacion(barbasco, cocimiento).
planta_preparacion(amapola_amarilla, infusion).
planta_preparacion(amate, horchata).
planta_preparacion(arnica, macera).
planta_preparacion(belladona, macera).
planta_preparacion(berro, infusion).
planta_preparacion(boldo, infusion).
planta_preparacion(borraja, infusion).
planta_preparacion(bugambilia, cocimiento).
planta_preparacion(cempasuchil, te).
planta_preparacion(chaparro_amargoso, te).
planta_preparacion(chicalote, cocimiento).
planta_preparacion(chile, compresas_calientes).
planta_preparacion(chichigua, decoccion).
planta_preparacion(cocolmeca, decoccion).
planta_preparacion(cola_de_caballo, infusion).
planta_preparacion(colchino, infusion_de_hojas).
planta_preparacion(colpachi, decoccion_de_corteza).
planta_preparacion(cuajiote, infusion).
planta_preparacion(cuasia, infusion).
planta_preparacion(diente_de_leon, infusion).
planta_preparacion(cilantro, infusion).
planta_preparacion(comino, infusion_de_semillas).
planta_preparacion(cuachalalate, infusion_o_decoccion_de_corteza).
planta_preparacion(digitaria, infusion).
planta_preparacion(doradilla, cocimiento).
planta_preparacion(enebro, infusion).
planta_preparacion(fenogreco, cocimiento).
planta_preparacion(geranio, infusion).
planta_preparacion(girasol, infusion).
planta_preparacion(gingseng, infusion).
planta_preparacion(grama, cocimiento).
planta_preparacion(guaco, infusion_cada_3_horas_o_cataplasma).
planta_preparacion(estafiate, infusion).
planta_preparacion(eucalipto, vaporizacion).
planta_preparacion(genciana, maceracion).
planta_preparacion(gordolobo, infusion).
planta_preparacion(guazuma, infusion_de_hojas).
planta_preparacion(guayacan, infusion_de_flores_o_corteza).
planta_preparacion(hamamelis, infusion_de_hojas).
planta_preparacion(helenio, infusion_de_raiz).
planta_preparacion(jengibre, tintura).
planta_preparacion(jengibre, pan).
planta_preparacion(jengibre, remedio_efervescente).
planta_preparacion(linaza, infusion).
planta_preparacion(linaza, cataplasma).
planta_preparacion(llanten, infusion).
planta_preparacion(llanten, leche_hervida).
planta_preparacion(madresilva, infusion).
planta_preparacion(maguey, cataplasma).
planta_preparacion(hierba_del_pollo, machacar).
planta_preparacion(hierba_del_pollo, infusion).
planta_preparacion(hinojo, infusion).
planta_preparacion(jalapa, cocimiento).
planta_preparacion(ipecacuana, infusion).
planta_preparacion(jazmin_amarillo, tintura).
planta_preparacion(maiz, infusion).
planta_preparacion(maiz, cataplasma).
planta_preparacion(maiz, consumo_directo).
planta_preparacion(malva, infusion).
planta_preparacion(malva, cataplasma).
planta_preparacion(malva, gargaras).
planta_preparacion(malvavisco, infusion_raiz).
planta_preparacion(malvavisco, jarabe).
planta_preparacion(malvavisco, cataplasma_hojas).
planta_preparacion(mangle, decoccion_corteza).
planta_preparacion(mangle, polvo_corteza).
planta_preparacion(mangle, cataplasma_hojas).
planta_preparacion(manzanilla, infusion_flores).
planta_preparacion(manzanilla, aceite_esencial).
planta_preparacion(manzanilla, compresas).
planta_preparacion(manzanilla, tintura_30gotas_agua).
planta_preparacion(marrubio, infusion).
planta_preparacion(marihuana, maceracion).
planta_preparacion(mastuerzo, cocimiento).
planta_preparacion(matarique, cocimiento).
planta_preparacion(palo_de_flor, cocimiento).
planta_preparacion(pinguica, cocimiento).
planta_preparacion(ruibarbo, cocimiento).
planta_preparacion(sen, infusion).
planta_preparacion(sanguinaria, cocimiento).
planta_preparacion(sensativa, infusion_hojas).
planta_preparacion(simonillo, te_flores).
planta_preparacion(tamarindo, infusion_vainas).
planta_preparacion(tamarindo, pulpa).
planta_preparacion(tabachin, infusion_flores).
planta_preparacion(taray, cocimiento_corteza).
planta_preparacion(regaliz, infusion).
planta_preparacion(retama, cocimiento).
planta_preparacion(ricino, maceracion).
planta_preparacion(rosal, infusion).
planta_preparacion(nopal, pencas).
planta_preparacion(nogal, hojas_y_corteza_en_te_o_infusion).
planta_preparacion(nuez_vomica, te_espanol).
planta_preparacion(nuez_vomica, hojas_te).
planta_preparacion(ocote, resina_mezclada).
planta_preparacion(ortiga, hojas_frescas_o_secas).
planta_preparacion(ortiga, infusion).
planta_preparacion(ortiga, cataplasma).
planta_preparacion(prodigiosa, cocimiento).
planta_preparacion(pirul, maceracion).
planta_preparacion(pulsatilla, infusion).
planta_preparacion(quebracho, infusion).
planta_preparacion(quina, infusion).
planta_preparacion(toloache, infusion).
planta_preparacion(tronadora, infusion).
planta_preparacion(tripa_de_judas, cocimiento).
planta_preparacion(uva, jugo_o_consumo_directo).
planta_preparacion(romero, infusion).
planta_preparacion(tila, infusion).
planta_preparacion(maiz, cocimiento).

como_ingerir(abrojo, 'usar la raiz en agua como agua uso').
como_ingerir(acacia, 'tomar te de sus hojas').
como_ingerir(acanto, 'hojas de acanto aplicadas en piel').
como_ingerir(aceitilla, 'tonico').
como_ingerir(achicoria, 'tomar te de sus hojas secas').
como_ingerir(sanguinaria, 'tomar en te 15 dias seguidos').
como_ingerir(sensitiva, 'cocer las hojas raiz y tomar en te').
como_ingerir(simonillo, 'usar toda la planta en te').
como_ingerir(tamarindo, 'por medio de agua de su pulpa').
como_ingerir(amapola_amarilla,'tomar el te de sus hojas solo durante la afeccion').
como_ingerir(anis,'tomarse como te').
como_ingerir(amate,'usar como emplastos en la sona de reumatismo').
como_ingerir(ruda, 'Tecito 2 o 3 veces por dia').
como_ingerir(ruibaro, 'Ingerir con pan bimbo untado con su mermelada').
como_ingerir(salvia, 'Se mastica para limpiar el aliento').
como_ingerir(sen, 'Se machaca la raiz para preparar un te').
como_ingerir(marihuana, 'frotaciones externas con un poco de alcanfor o tomar como agua de uso').
como_ingerir(matarique, 'tomar en te o como tintura').
como_ingerir(digitaria, 'tomar en te bajo supervision medica').
como_ingerir(cardo_santo, 'aplicar dos gotas en cada ojo').
como_ingerir(brionia, 'tomar en te con precaucion por su toxicidad').
como_ingerir(canela, 'tomar en te o agregar a alimentos').
como_ingerir(cedron, 'tomar en te').
como_ingerir(doradilla, 'tomar en te').
como_ingerir(epazote, 'tomar en te').
como_ingerir(enebro, 'tomar en te o usar en lavados').
como_ingerir(mangle, 'tomar en te o aplicar en heridas').
como_ingerir(manzanilla, 'tomar en te').
como_ingerir(marrubio, 'tomar en te').
como_ingerir(mastuerzo, 'tomar en te o aplicar en compresas').
como_ingerir(aconito, 'Aplicar tintura externamente con precaución').
como_ingerir(adormidera, 'Tomar infusión en pequeñas dosis').
como_ingerir(ahuehuete, 'Usar decocción para lavados o compresas').
como_ingerir(ajo, 'Consumir con miel, machacado o molido con leche').
como_ingerir(albahaca, 'Friccionar jugo en el cuero cabelludo').
como_ingerir(alcachofa, 'Tomar cocimiento de las hojas').
como_ingerir(abrojo, 'Tomar cocimiento tres veces al día').
como_ingerir(aguacate, 'Tomar infusión de hojas').
como_ingerir(ajenjo, 'Tomar infusión en pequeñas dosis').
como_ingerir(alcanfor, 'Untar aceite tres veces al día').
como_ingerir(menta, 'tomar en te').
como_ingerir(anacahuite, 'Tomar cocimiento para afecciones respiratorias').
como_ingerir(barbasco, 'Aplicar cocimiento externamente con precaucion').
como_ingerir(amapola_amarilla, 'Tomar infusion una vez por la noche').
como_ingerir(amate, 'Beber horchata cada 8 horas').
como_ingerir(arnica, 'Aplicar macerado externamente en la zona afectada').
como_ingerir(belladona, 'Usar macerado en dosis minimas bajo supervision medica').
como_ingerir(berro, 'Tomar infusion dos veces al dia').
como_ingerir(boldo, 'Tomar infusion despues de las comidas').
como_ingerir(borraja, 'Tomar infusion dos o tres veces al dia').
como_ingerir(bugambilia, 'Tomar cocimiento tres veces al dia').
como_ingerir(cempasuchil, 'Tomar te en caso de parasitos').
como_ingerir(chaparro_amargoso, 'Tomar te de hojas y corteza dos veces al dia').
como_ingerir(chicalote, 'Tomar cocimiento o aplicar leche directamente en ojos').
como_ingerir(chile, 'Aplicar hojas en pecho o panos calientes con cocimiento').
como_ingerir(chichigua, 'Aplicar decoccion topicamente dos veces al dia').
como_ingerir(cocolmeca, 'Tomar decoccion una vez al dia').
como_ingerir(cola_de_caballo, 'Tomar infusion una a dos veces al dia').
como_ingerir(colchino, 'Tomar infusion de hojas dos veces al dia').
como_ingerir(colpachi, 'Aplicar infusion tibia en piel dos veces al dia').
como_ingerir(cuajiote, 'Tomar infusion con moderacion por no mas de 3 dias').
como_ingerir(cuasia, 'Tomar infusion una taza por dia').
como_ingerir(diente_de_leon, 'Tomar infusion en ayunas').
como_ingerir(cilantro, 'Tomar infusion dos veces al dia').
como_ingerir(comino, 'Tomar infusion de semillas tras las comidas').
como_ingerir(cuachalalate, 'Tomar infusion o decoccion tres veces al dia tras comidas').
como_ingerir(digitaria, 'Tomar infusion bajo supervision medica').
como_ingerir(doradilla, 'Tomar cocimiento una o dos veces al dia').
como_ingerir(enebro, 'Tomar infusion o usar para lavados externos').
como_ingerir(fenogreco, 'Tomar cocimiento en ayunas').
como_ingerir(geranio, 'Tomar infusion antes de dormir').
como_ingerir(girasol, 'Tomar te diario o aplicar compresas').
como_ingerir(gingseng, 'Tomar infusion una vez al dia').
como_ingerir(grama, 'Tomar te o jarabe una vez al dia').
como_ingerir(guaco, 'Tomar infusion cada 3 horas o aplicar cataplasma').
como_ingerir(estafiate, 'Tomar infusion despues de los alimentos').
como_ingerir(eucalipto, 'Inhalar vapor de hojas hervidas').
como_ingerir(genciana, 'Tomar macerado media taza antes de comidas').
como_ingerir(gordolobo, 'Tomar infusion 2-3 veces al dia').
como_ingerir(guazuma, 'Tomar infusion colada y fria').
como_ingerir(guayacan, 'Tomar infusion durante 5 dias, descansar 5 dias').
como_ingerir(hamamelis, 'Tomar infusion reposada 15 minutos').
como_ingerir(helenio, 'Tomar infusion diaria').
como_ingerir(jengibre, 'Tomar tintura, pan o remedio efervescente internamente').
como_ingerir(linaza, 'Tomar infusion o aplicar cataplasma externamente').
como_ingerir(llanten, 'Tomar infusion, leche hervida o aplicar externamente').
como_ingerir(madresilva, 'Tomar infusion internamente').
como_ingerir(maguey, 'Aplicar cataplasma de hojas externamente').
como_ingerir(hierba_del_pollo, 'Aplicar localmente o tomar infusion').
como_ingerir(hinojo, 'Tomar infusion despues de comidas').
como_ingerir(jalapa, 'Tomar cocimiento en ayunas').
como_ingerir(ipecacuana, 'Tomar infusion para la tos').
como_ingerir(jazmin_amarillo, 'Tomar tintura para dolores o espasmos').
como_ingerir(maiz, 'Tomar infusion, aplicar cataplasma o consumir hervido').
como_ingerir(malva, 'Tomar infusion tres veces al dia, aplicar cataplasma o hacer gargaras').
como_ingerir(malvavisco, 'Tomar jarabe 4 veces al dia, infusion fria o aplicar compresas').
como_ingerir(mangle, 'Tomar decoccion, aplicar polvo en heridas o masticar propagulos').
como_ingerir(manzanilla, 'Tomar infusion, aplicar compresas, usar aceite diluido o tintura').
como_ingerir(marrubio, 'Tomar infusion dos veces al dia').
como_ingerir(marihuana, 'Aplicar maceracion externamente bajo supervision medica').
como_ingerir(mastuerzo, 'Tomar cocimiento o aplicar compresas').
como_ingerir(matarique, 'Tomar cocimiento o tintura para diabetes o reumatismo').
como_ingerir(palo_de_flor, 'Tomar cocimiento dos veces al dia').
como_ingerir(pinguica, 'Tomar cocimiento tres veces al dia').
como_ingerir(ruibarbo, 'Tomar cocimiento en pequenas dosis').
como_ingerir(sen, 'Tomar infusion una taza diaria por una semana').
como_ingerir(sanguinaria, 'Tomar cocimiento o hacer gargaras').
como_ingerir(sensativa, 'Tomar infusion de hojas por la noche').
como_ingerir(simonillo, 'Tomar te de flores en ayunas').
como_ingerir(tamarindo, 'Tomar infusion de vainas o consumir pulpa cuando sea necesario').
como_ingerir(tabachin, 'Tomar infusion de flores tres veces al dia').
como_ingerir(taray, 'Tomar cocimiento como agua de uso').
como_ingerir(regaliz, 'Tomar infusion en pequenas cantidades').
como_ingerir(menta, 'Tomar infusion dos o tres veces al dia').
como_ingerir(oregano, 'Tomar infusion dos veces al dia').
como_ingerir(pasiflora, 'Tomar infusion antes de dormir').
como_ingerir(pericon, 'Tomar infusion despues de las comidas').
como_ingerir(ruda, 'Tomar infusion con moderacion').
como_ingerir(salvia, 'Hacer gargaras o tomar infusion').
como_ingerir(retama, 'Tomar cocimiento con precaucion').
como_ingerir(ricino, 'Usar maceracion externamente o como laxante ocasional').
como_ingerir(rosal, 'Tomar infusion dos tazas al dia').
como_ingerir(nopal, 'Consumir pencas directamente o aplicar cataplasma').
como_ingerir(nogal, 'Tomar te, infusion o consumir directamente').
como_ingerir(nuez_vomica, 'Tomar te espanol o infusion de hojas').
como_ingerir(ocote, 'Aplicar cataplasma de resina mezclada').
como_ingerir(ortiga, 'Tomar te, infusion, aplicar cataplasma o consumir hojas').
como_ingerir(prodigiosa, 'Tomar cocimiento en ayunas y antes de comidas').
como_ingerir(pirul, 'Tomar 20 gotas de maceracion cada 8 horas').
como_ingerir(pulsatilla, 'Tomar infusion tres a cinco veces al dia').
como_ingerir(quebracho, 'Tomar infusion una a tres tazas al dia').
como_ingerir(quina, 'Tomar infusion una a dos tazas al dia').
como_ingerir(toloache, 'Usar infusion externamente o en dosis baja').
como_ingerir(tronadora, 'Tomar infusion dos veces al dia').
como_ingerir(tripa_de_judas, 'Tomar cocimiento tres veces al dia').
como_ingerir(uva, 'Consumir jugo o uvas diariamente').
como_ingerir(romero, 'Tomar infusion una taza por la manana').
como_ingerir(tila, 'Tomar infusion una vez por la noche').
como_ingerir(maiz, 'Tomar cocimiento dos veces al dia o aplicar cataplasma').

origen_planta(abrojo, 'Europa').
origen_planta(acacia, 'Africa').
origen_planta(acanto, 'Europa').
origen_planta(aceitilla, 'America').
origen_planta(achicoria, 'Europa').
origen_planta(tamarindo, 'India').
origen_planta(amapola_amarilla, 'Europa').
origen_planta(amate, 'America').
origen_planta(anis, 'Asia').
origen_planta(ruda, 'Europa').
origen_planta(ruibaro, 'Asia').
origen_planta(salvia, 'America').
origen_planta(sen, 'Africa').
origen_planta(marihuana, 'India').
origen_planta(matarique, 'Norte de Mexico').
origen_planta(digitaria, 'Europa').
origen_planta(cardo_santo, 'Mexico').
origen_planta(brionia, 'Europa').
origen_planta(canela, 'Asia').
origen_planta(cedron, 'America').
origen_planta(doradilla, 'America').
origen_planta(epazote, 'America').
origen_planta(enebro, 'Europa').
origen_planta(mangle, 'America').
origen_planta(manzanilla, 'Europa').
origen_planta(marrubio, 'Europa').
origen_planta(mastuerzo, 'Asia').
origen_planta(menta, 'Europa').
origen_planta(aconito, 'Europa').
origen_planta(adormidera, 'Asia').
origen_planta(ahuehuete, 'Mexico').
origen_planta(ajo, 'Asia Central').
origen_planta(albahaca, 'Asia Central y Africa').
origen_planta(alcachofa, 'Africa').
origen_planta(anacahuite, 'America').
origen_planta(barbasco, 'America').
origen_planta(bugambilia, 'America').
origen_planta(cempasuchil, 'America').
origen_planta(chaparro_amargoso, 'America').
origen_planta(chicalote, 'America').
origen_planta(chile, 'America').
origen_planta(chichigua, 'America').
origen_planta(cocolmeca, 'America').
origen_planta(cola_de_caballo, 'Europa').
origen_planta(colchino, 'Mexico').
origen_planta(colpachi, 'Mexico').
origen_planta(cuajiote, 'America').
origen_planta(cuasia, 'America').
origen_planta(diente_de_leon, 'Europa y Asia').
origen_planta(digitaria, 'Europa').
origen_planta(doradilla, 'America').
origen_planta(enebro, 'Europa').
origen_planta(fenogreco, 'Asia').
origen_planta(geranio, 'Africa').
origen_planta(girasol, 'America').
origen_planta(gingseng, 'Asia').
origen_planta(grama, 'Africa').
origen_planta(guaco, 'America').
origen_planta(guazuma, 'America').
origen_planta(guayacan, 'America').
origen_planta(hamamelis, 'America').
origen_planta(helenio, 'Europa').
origen_planta(madresilva, 'Desconocido').
origen_planta(maguey, 'America').
origen_planta(hierba_del_pollo, 'America').
origen_planta(jalapa, 'America').
origen_planta(ipecacuana, 'America').
origen_planta(jazmin_amarillo, 'America').
origen_planta(malva, 'Europa').
origen_planta(malvavisco, 'Europa').
origen_planta(mangle, 'America').
origen_planta(marrubio, 'Europa').
origen_planta(marihuana, 'Asia').
origen_planta(mastuerzo, 'Asia').
origen_planta(matarique, 'America').
origen_planta(palo_de_flor, 'America').
origen_planta(pinguica, 'America').
origen_planta(ruibarbo, 'Asia').
origen_planta(sen, 'Africa').
origen_planta(sanguinaria, 'America').
origen_planta(sensativa, 'America').
origen_planta(simonillo, 'America').
origen_planta(tabachin, 'America').
origen_planta(taray, 'Asia').
origen_planta(regaliz, 'Europa').
origen_planta(retama, 'Europa').
origen_planta(ricino, 'Africa').
origen_planta(rosal, 'Asia').
origen_planta(nogal, 'Europa').
origen_planta(nuez_vomica, 'Oceania').
origen_planta(ocote, 'America').
origen_planta(ortiga, 'Europa').
origen_planta(prodigiosa, 'America').
origen_planta(pirul, 'America').
origen_planta(pulsatilla, 'Europa').
origen_planta(quebracho, 'America').
origen_planta(quina, 'America').
origen_planta(toloache, 'America').
origen_planta(tronadora, 'America').
origen_planta(tripa_de_judas, 'America').
origen_planta(uva, 'Europa').
origen_planta(nopal, 'America').

nombre_cientifico(abrojo, 'Tribulus terrestris').
nombre_cientifico(acacia, 'Acacia spp.').
nombre_cientifico(acanto, 'Acanthus mollis').
nombre_cientifico(aceitilla, 'Bidens pilosa').
nombre_cientifico(achicoria, 'Cichorium intybus').
nombre_cientifico(sanguinaria, 'Polygonum aviculare').
nombre_cientifico(sensitiva, 'Mimosa pudica').
nombre_cientifico(simonillo, 'Centaurea aspera').
nombre_cientifico(tamarindo, 'Tamarindus indica').
nombre_cientifico(amapola_amarilla, 'Eschscholzia californica').
nombre_cientifico(anis, 'Pimpinella anisum').
nombre_cientifico(amate, 'Ficus insipida').
nombre_cientifico(ruda, 'Ruta graveolens').
nombre_cientifico(ruibaro, 'Rheum rhabarbarum').
nombre_cientifico(salvia, 'Salvia officinalis').
nombre_cientifico(sen, 'Senna alexandrina').
nombre_cientifico(marihuana, 'Cannabis sativa').
nombre_cientifico(matarique, 'Psacalium decompositum').
nombre_cientifico(digitaria, 'Digitalis purpurea').
nombre_cientifico(cardo_santo, 'Cnicus benedictus').
nombre_cientifico(brionia, 'Bryonia alba').
nombre_cientifico(canela, 'Cinnamomum verum').
nombre_cientifico(cedron, 'Aloysia citrodora').
nombre_cientifico(doradilla, 'Ceterach officinarum').
nombre_cientifico(epazote, 'Dysphania ambrosioides').
nombre_cientifico(enebro, 'Juniperus communis').
nombre_cientifico(mangle, 'Rhizophora mangle').
nombre_cientifico(manzanilla, 'Matricaria chamomilla').
nombre_cientifico(marrubio, 'Marrubium vulgare').
nombre_cientifico(mastuerzo, 'Lepidium sativum').
nombre_cientifico(menta, 'Mentha piperita').
nombre_cientifico(aconito, 'Aconitum napellus').
nombre_cientifico(adormidera, 'Papaver somniferum').
nombre_cientifico(ahuehuete, 'Taxodium mucronatum').
nombre_cientifico(ajo, 'Allium sativum').
nombre_cientifico(albahaca, 'Ocimum basilicum').
nombre_cientifico(alcachofa, 'Cynara scolymus').
nombre_cientifico(anacahuite, 'Cordia boissieri').
nombre_cientifico(barbasco, 'Jacquinia arborea').
nombre_cientifico(bugambilia, 'Bougainvillea glabra').
nombre_cientifico(cempasuchil, 'Tagetes erecta').
nombre_cientifico(chaparro_amargoso, 'Castela americana').
nombre_cientifico(chicalote, 'Argemone aecholtzia').
nombre_cientifico(chile, 'Capsicum annuum').
nombre_cientifico(chichigua, 'Solanum mammosum').
nombre_cientifico(cocolmeca, 'Smilax spinosa').
nombre_cientifico(cola_de_caballo, 'Equisetum arvensis').
nombre_cientifico(colchino, 'Argemone mexicana').
nombre_cientifico(colpachi, 'Croton glabellus').
nombre_cientifico(cuajiote, 'Bursera morelense').
nombre_cientifico(cuasia, 'Quassia amara').
nombre_cientifico(diente_de_leon, 'Taraxacum officinale').
nombre_cientifico(digitaria, 'Digitalis purpurea').
nombre_cientifico(doradilla, 'Ceterach officinarum').
nombre_cientifico(enebro, 'Juniperus communis').
nombre_cientifico(fenogreco, 'Trigonella foenum-graecum').
nombre_cientifico(geranio, 'Pelargonium graveolens').
nombre_cientifico(girasol, 'Helianthus annuus').
nombre_cientifico(gingseng, 'Panax ginseng').
nombre_cientifico(grama, 'Cynodon dactylon').
nombre_cientifico(guaco, 'Aristolochia odoratissima').
nombre_cientifico(guazuma, 'Guazuma ulmifolia').
nombre_cientifico(guayacan, 'Guaiacum sanctum').
nombre_cientifico(hamamelis, 'Hamamelis virginica').
nombre_cientifico(helenio, 'Inula helenium').
nombre_cientifico(madresilva, 'Lonicera').
nombre_cientifico(maguey, 'Agave').
nombre_cientifico(hierba_del_pollo, 'Tradescantia zebrina').
nombre_cientifico(jalapa, 'Ipomea purga').
nombre_cientifico(ipecacuana, 'Polygala hondurana').
nombre_cientifico(jazmin_amarillo, 'Gelsemium sempervirens').
nombre_cientifico(malva, 'Malva sylvestris').
nombre_cientifico(malvavisco, 'Althaea officinalis').
nombre_cientifico(mangle, 'Rhizophora mangle').
nombre_cientifico(marrubio, 'Marrubium vulgare').
nombre_cientifico(marihuana, 'Cannabis sativa').
nombre_cientifico(mastuerzo, 'Lepidium sativum').
nombre_cientifico(matarique, 'Psacalium decompositum').
nombre_cientifico(palo_de_flor, 'Bourreria huanita').
nombre_cientifico(pinguica, 'Arctostaphylos pungens').
nombre_cientifico(ruibarbo, 'Rheum palmatum').
nombre_cientifico(sen, 'Senna alexandrina').
nombre_cientifico(sanguinaria, 'Sanguinaria canadensis').
nombre_cientifico(sensativa, 'Mimosa pudica').
nombre_cientifico(simonillo, 'Tagetes lucida').
nombre_cientifico(tabachin, 'Cesalpinia pulcherrima').
nombre_cientifico(taray, 'Tamarix aphylla').
nombre_cientifico(regaliz, 'Glycyrrhiza glabra').
nombre_cientifico(retama, 'Spartium junceum').
nombre_cientifico(ricino, 'Ricinus communis').
nombre_cientifico(rosal, 'Rosa centifolia').
nombre_cientifico(nogal, 'Juglans regia').
nombre_cientifico(nuez_vomica, 'Strychnos nux-vomica').
nombre_cientifico(ocote, 'Pinus teocote').
nombre_cientifico(ortiga, 'Urtica urens').
nombre_cientifico(prodigiosa, 'Coleosanthus squarrosus').
nombre_cientifico(pirul, 'Schinus molle').
nombre_cientifico(pulsatilla, 'Anemone pulsatilla').
nombre_cientifico(quebracho, 'Lysiloma auritum').
nombre_cientifico(quina, 'Cinchona calisaya').
nombre_cientifico(toloache, 'Datura stramonium').
nombre_cientifico(tronadora, 'Tecoma stans').
nombre_cientifico(tripa_de_judas, 'Leonotis nepetifolia').
nombre_cientifico(uva, 'Vitis vinifera').

modo_tratamiento(abrojo, 'Tomar como agua de uso diariamente').
modo_tratamiento(acacia, 'Tomar te de hojas 2-3 veces al dia').
modo_tratamiento(acanto, 'Aplicar hojas frescas sobre la piel afectada').
modo_tratamiento(aceitilla, 'Consumir como tonico una vez al dia').
modo_tratamiento(achicoria, 'Tomar te de hojas secas 2 veces al dia').
modo_tratamiento(sanguinaria, 'Tomar te durante 15 dias consecutivos').
modo_tratamiento(sensitiva, 'Tomar te de hojas y raices 1-2 veces al dia').
modo_tratamiento(simonillo, 'Tomar te de la planta entera 2 veces al dia').
modo_tratamiento(tamarindo, 'Consumir agua de pulpa 1-2 veces al dia').
modo_tratamiento(amapola_amarilla, 'Tomar te de hojas durante la afeccion').
modo_tratamiento(anis, 'Tomar te 2-3 veces al dia despues de comidas').
modo_tratamiento(amate, 'Aplicar emplastos en la zona afectada por reumatismo').
modo_tratamiento(ruda, 'Tomar te 2-3 veces al dia').
modo_tratamiento(ruibaro, 'Consumir mermelada untada en pan diariamente').
modo_tratamiento(salvia, 'Masticar hojas frescas para higiene bucal').
modo_tratamiento(sen, 'Tomar te de raiz machacada 1 vez al dia').
modo_tratamiento(marihuana, 'Aplicar frotaciones externas o tomar como agua de uso').
modo_tratamiento(matarique, 'Tomar te o tintura 1-2 veces al dia').
modo_tratamiento(digitaria, 'Tomar te bajo supervision medica estricta').
modo_tratamiento(cardo_santo, 'Aplicar 2 gotas en cada ojo, 1-2 veces al dia').
modo_tratamiento(brionia, 'Tomar te en dosis bajas con supervision medica').
modo_tratamiento(canela, 'Tomar te o anadir a alimentos diariamente').
modo_tratamiento(cedron, 'Tomar te 2-3 veces al dia').
modo_tratamiento(doradilla, 'Tomar te 1-2 veces al dia').
modo_tratamiento(epazote, 'Tomar te 1 vez al dia').
modo_tratamiento(enebro, 'Tomar te o usar en lavados externos').
modo_tratamiento(mangle, 'Tomar te o aplicar en heridas 1-2 veces al dia').
modo_tratamiento(manzanilla, 'Tomar te 2-3 veces al dia').
modo_tratamiento(marrubio, 'Tomar te 2 veces al dia').
modo_tratamiento(mastuerzo, 'Tomar te o aplicar compresas 1-2 veces al dia').
modo_tratamiento(menta, 'Tomar te 2-3 veces al dia').
modo_tratamiento(aconito, 'Aplicar externamente bajo supervisión médica').
modo_tratamiento(adormidera, 'Tomar infusión 1 vez al día con precaución').
modo_tratamiento(ahuehuete, 'Usar decocción 1-2 veces al día externamente').
modo_tratamiento(ajo, 'Consumir en ayunas o aplicar directamente').
modo_tratamiento(albahaca, 'Friccionar en el cuero cabelludo diariamente').
modo_tratamiento(alcachofa, 'Tomar cocimiento con gusto 1-2 veces al día').
modo_tratamiento(abrojo, 'Tomar cocimiento tres veces al día').
modo_tratamiento(aguacate, 'Tomar infusión 1-2 veces al día').
modo_tratamiento(ajenjo, 'Tomar infusión 1 vez al día con precaución').
modo_tratamiento(alcanfor, 'Untar aceite 3 veces al día').
modo_tratamiento(anacahuite, 'Tomar cocimiento 1-2 veces al dia').
modo_tratamiento(barbasco, 'Aplicar cocimiento externamente 1 vez al dia').
modo_tratamiento(amapola_amarilla, 'Tomar infusion 1 vez por la noche').
modo_tratamiento(amate, 'Beber horchata cada 8 horas').
modo_tratamiento(arnica, 'Aplicar macerado externamente 1-2 veces al dia').
modo_tratamiento(belladona, 'Usar macerado en dosis minimas bajo supervision medica').
modo_tratamiento(berro, 'Tomar infusion dos veces al dia').
modo_tratamiento(boldo, 'Tomar infusion despues de las comidas').
modo_tratamiento(borraja, 'Tomar infusion dos o tres veces al dia').
modo_tratamiento(bugambilia, 'Tomar cocimiento tres veces al dia').
modo_tratamiento(brionia, 'Tomar te en dosis bajas con supervision medica').
modo_tratamiento(canela, 'Tomar te o anadir a alimentos diariamente').
modo_tratamiento(cedron, 'Tomar te dos o tres veces al dia').
modo_tratamiento(cardo_santo, 'Aplicar 2 gotas en cada ojo 1-2 veces al dia').
modo_tratamiento(cempasuchil, 'Tomar te en caso de parasitos, hasta 3 veces al dia').
modo_tratamiento(chaparro_amargoso, 'Tomar te de hojas y corteza 2 veces al dia, o usar como lavativa en disenteria cronica').
modo_tratamiento(chicalote, 'Tomar cocimiento 2 veces al dia o aplicar leche en ojos para carnosidad').
modo_tratamiento(chile, 'Aplicar hojas en pecho para asma o panos calientes para reumatismo').
modo_tratamiento(chichigua, 'Aplicar decoccion topicamente dos veces al dia').
modo_tratamiento(cocolmeca, 'Tomar decoccion una vez al dia').
modo_tratamiento(cola_de_caballo, 'Tomar infusion una a dos veces al dia').
modo_tratamiento(colchino, 'Tomar infusion dos veces al dia por 5 dias').
modo_tratamiento(colpachi, 'Aplicar infusion tibia en piel 2 veces al dia').
modo_tratamiento(cuajiote, 'Tomar infusion con moderacion por no mas de 3 dias').
modo_tratamiento(cuasia, 'Tomar infusion una taza por dia').
modo_tratamiento(diente_de_leon, 'Tomar infusion en ayunas').
modo_tratamiento(cilantro, 'Tomar infusion dos veces al dia').
modo_tratamiento(comino, 'Tomar infusion tras las comidas').
modo_tratamiento(cuachalalate, 'Tomar infusion 3 veces al dia tras las comidas').
modo_tratamiento(damiana, 'Tomar de una a dos tazas al dia').
modo_tratamiento(digitaria, 'Tomar infusion bajo supervision medica').
modo_tratamiento(doradilla, 'Tomar cocimiento una o dos veces al dia').
modo_tratamiento(epazote, 'Tomar cocimiento una vez al dia').
modo_tratamiento(enebro, 'Tomar infusion o realizar lavados externos').
modo_tratamiento(fenogreco, 'Tomar cocimiento en ayunas').
modo_tratamiento(geranio, 'Tomar infusion antes de dormir').
modo_tratamiento(girasol, 'Tomar te diario o aplicar compresas').
modo_tratamiento(gingseng, 'Tomar infusion una vez al dia').
modo_tratamiento(grama, 'Tomar te o jarabe una vez al dia').
modo_tratamiento(guaco, 'Tomar infusion cada 3 horas o aplicar cataplasma externamente').
modo_tratamiento(estafiate, 'Tomar infusion una taza despues de los alimentos').
modo_tratamiento(eucalipto, 'Inhalar vapor 2 veces al dia').
modo_tratamiento(genciana, 'Tomar macerado media taza antes de las comidas').
modo_tratamiento(gordolobo, 'Tomar infusion 2-3 veces al dia').
modo_tratamiento(guazuma, 'Tomar infusion colada y fria').
modo_tratamiento(guayacan, 'Tomar infusion durante 5 dias, descansar 5 dias').
modo_tratamiento(hamamelis, 'Tomar infusion reposada 15 minutos').
modo_tratamiento(helenio, 'Tomar infusion diaria').
modo_tratamiento(jengibre, 'Uso interno de tintura, pan o remedio efervescente').
modo_tratamiento(linaza, 'Tomar infusion o aplicar cataplasma para heridas o abscesos').
modo_tratamiento(llanten, 'Tomar infusion, leche hervida o aplicar externamente').
modo_tratamiento(madresilva, 'Tomar infusion para gripa o infecciones').
modo_tratamiento(maguey, 'Aplicar cataplasma externamente para llagas o infecciones').
modo_tratamiento(hierba_del_pollo, 'Aplicar localmente para hemorragias o tomar te').
modo_tratamiento(hinojo, 'Tomar infusion para gases o flatulencias').
modo_tratamiento(jalapa, 'Tomar cocimiento en ayunas').
modo_tratamiento(ipecacuana, 'Tomar infusion para la tos').
modo_tratamiento(jazmin_amarillo, 'Tomar tintura para dolores, espasmos o asma').
modo_tratamiento(maiz, 'Tomar infusion tres veces al dia, aplicar cataplasma o consumir hervido').
modo_tratamiento(malva, 'Tomar infusion tres veces al dia, aplicar cataplasma o hacer gargaras dos veces al dia').
modo_tratamiento(malvavisco, 'Tomar jarabe 4 veces al dia, infusion de raiz fria o compresas de raiz molida').
modo_tratamiento(mangle, 'Tomar decoccion de 50g corteza por litro, aplicar polvo en heridas o masticar propagulos').
modo_tratamiento(manzanilla, 'Tomar infusion de 3g flores en 150ml, compresas en ojos, aceite diluido o tintura 30 gotas').
modo_tratamiento(marrubio, 'Tomar infusion dos veces al dia').
modo_tratamiento(marihuana, 'Frotaciones externas con maceracion bajo supervision medica').
modo_tratamiento(mastuerzo, 'Tomar cocimiento o aplicar compresas para ciatica o tuberculosis').
modo_tratamiento(matarique, 'Tomar cocimiento o tintura para diabetes o reumatismo').
modo_tratamiento(palo_de_flor, 'Tomar cocimiento dos veces al dia').
modo_tratamiento(pinguica, 'Tomar cocimiento tres veces al dia').
modo_tratamiento(ruibarbo, 'Tomar cocimiento en pequenas dosis').
modo_tratamiento(sen, 'Tomar infusion una taza diaria por una semana').
modo_tratamiento(sanguinaria, 'Tomar cocimiento o hacer gargaras para problemas respiratorios').
modo_tratamiento(sensativa, 'Tomar infusion de hojas por la noche para ansiedad o insomnio').
modo_tratamiento(simonillo, 'Tomar te de flores en ayunas para problemas estomacales').
modo_tratamiento(tamarindo, 'Tomar infusion de vainas o consumir pulpa cuando sea necesario').
modo_tratamiento(tabachin, 'Tomar infusion de flores tres veces al dia').
modo_tratamiento(taray, 'Tomar cocimiento de corteza como agua de uso').
modo_tratamiento(regaliz, 'Tomar infusion en pequenas cantidades para tos o digestion').
modo_tratamiento(menta, 'Tomar infusion dos o tres veces al dia para nauseas o insomnio').
modo_tratamiento(oregano, 'Tomar infusion dos veces al dia para digestion o resfriado').
modo_tratamiento(pasiflora, 'Tomar infusion antes de dormir para insomnio o ansiedad').
modo_tratamiento(pericon, 'Tomar infusion despues de las comidas para colicos o indigestion').
modo_tratamiento(ruda, 'Tomar infusion con moderacion para menstruacion o nerviosismo').
modo_tratamiento(salvia, 'Hacer gargaras o tomar infusion para dolor de garganta o digestion').
modo_tratamiento(retama, 'Tomar cocimiento con precaucion por toxicidad').
modo_tratamiento(ricino, 'Uso externo de maceracion o laxante ocasional').
modo_tratamiento(rosal, 'Tomar infusion dos tazas al dia para estres o digestion').
modo_tratamiento(nopal, 'Consumir pencas directamente o aplicar cataplasma para heridas').
modo_tratamiento(nogal, 'Tomar te, infusion o consumir directamente para anemia o herpes').
modo_tratamiento(nuez_vomica, 'Tomar te espanol o infusion de hojas para fiebres o bronquitis').
modo_tratamiento(ocote, 'Aplicar cataplasma de resina para problemas respiratorios o dolor').
modo_tratamiento(ortiga, 'Tomar te, infusion, aplicar cataplasma o consumir hojas para artritis o anemia').
modo_tratamiento(prodigiosa, 'Tomar cocimiento una taza en ayunas y antes de cada comida').
modo_tratamiento(pirul, 'Tomar 20 gotas de maceracion cada 8 horas para gonorrea').
modo_tratamiento(pulsatilla, 'Tomar infusion tres a cinco veces al dia para herpes o tos').
modo_tratamiento(quebracho, 'Tomar infusion una a tres tazas al dia para inflamaciones').
modo_tratamiento(quina, 'Tomar infusion una a dos tazas al dia para asma o epilepsia').
modo_tratamiento(toloache, 'Usar infusion externamente o en dosis baja para dolor o asma').
modo_tratamiento(tronadora, 'Tomar infusion dos veces al dia para diabetes o fiebre').
modo_tratamiento(tripa_de_judas, 'Tomar cocimiento tres veces al dia para tos o gripe').
modo_tratamiento(uva, 'Consumir jugo o uvas diariamente para estrenimiento o hipertension').
modo_tratamiento(romero, 'Tomar infusion una taza por la manana para fatiga o digestion').
modo_tratamiento(tila, 'Tomar infusion una vez por la noche para insomnio o ansiedad').
modo_tratamiento(maiz, 'Tomar cocimiento dos veces al dia para infeccion urinaria o retencion').

precauciones(abrojo, 'Evitar en embarazo y lactancia. Consultar medico si hay problemas renales.').
precauciones(acacia, 'No exceder la dosis recomendada. Puede causar alergias.').
precauciones(acanto, 'Evitar contacto prolongado con la piel sensible.').
precauciones(aceitilla, 'Consultar medico en casos de depresion severa.').
precauciones(achicoria, 'No usar en exceso, puede causar molestias gastricas.').
precauciones(sanguinaria, 'No exceder los 15 dias de uso continuo. Consultar medico.').
precauciones(sensitiva, 'Evitar en hipertensos o personas con alergias.').
precauciones(simonillo, 'No usar en casos de alergia a las asteraceas.').
precauciones(tamarindo, 'Evitar en personas con problemas renales o diabetes.').
precauciones(amapola_amarilla, 'Usar solo durante la afeccion. Evitar en ninos.').
precauciones(anis, 'Evitar en casos de alergia o problemas hepaticos.').
precauciones(amate, 'Consultar medico antes de usar en reumatismo cronico.').
precauciones(ruda, 'Toxica en dosis altas. Evitar en embarazo y lactancia.').
precauciones(ruibaro, 'No consumir en exceso, puede causar diarrea.').
precauciones(salvia, 'Evitar en hipertensos o personas con epilepsia.').
precauciones(sen, 'No usar en ninos ni en casos de obstruccion intestinal.').
precauciones(marihuana, 'Uso bajo supervision medica. Evitar en menores de edad.').
precauciones(matarique, 'Consultar medico en casos de diabetes o problemas renales.').
precauciones(digitaria, 'Toxica en dosis altas. Usar solo bajo supervision medica.').
precauciones(cardo_santo, 'Evitar contacto prolongado con los ojos. Consultar medico.').
precauciones(brionia, 'Toxica en dosis altas. Usar bajo supervision medica.').
precauciones(canela, 'Evitar en personas con alergias o problemas de coagulacion.').
precauciones(cedron, 'Evitar en personas con presion arterial baja.').
precauciones(doradilla, 'No usar en exceso, puede causar irritacion renal.').
precauciones(epazote, 'Evitar en embarazo y ninos pequenos. Consultar medico.').
precauciones(enebro, 'No usar en personas con problemas renales o embarazo.').
precauciones(mangle, 'Evitar en personas con alergias a plantas astringentes.').
precauciones(manzanilla, 'Evitar en personas alergicas a las asteraceas.').
precauciones(marrubio, 'No usar en exceso, puede causar molestias estomacales.').
precauciones(mastuerzo, 'Consultar medico en casos de tuberculosis avanzada.').
precauciones(menta, 'Evitar en personas con reflujo gastroesofagico.').
precauciones(aconito, 'Toxicidad alta. Usar solo bajo supervisión médica.').
precauciones(adormidera, 'Riesgo de dependencia. Evitar en niños y embarazadas.').
precauciones(ahuehuete, 'Evitar en pieles sensibles. Consultar médico si hay alergias.').
precauciones(ajo, 'Evitar en personas con problemas gastrointestinales.').
precauciones(albahaca, 'Puede causar reacciones alérgicas en piel sensible.').
precauciones(alcachofa, 'Evitar en personas con alergias a asteráceas.').
precauciones(abrojo, 'Evitar en embarazo y lactancia. Consultar médico si hay problemas renales.').
precauciones(aguacate, 'Evitar en personas con alergias a látex o frutas.').
precauciones(ajenjo, 'Evitar en embarazo y personas con epilepsia.').
precauciones(alcanfor, 'No usar en exceso, puede causar irritación.').
precauciones(anacahuite, 'Consultar medico si persisten los sintomas respiratorios').
precauciones(barbasco, 'Toxico, usar con precaucion y evitar ingestion').
precauciones(amapola_amarilla, 'Evitar en embarazo y lactancia').
precauciones(amate, 'Puede irritar la piel, usar con precaucion').
precauciones(arnica, 'Evitar en pieles sensibles o heridas abiertas').
precauciones(belladona, 'Toxica, usar solo bajo supervision medica').
precauciones(berro, 'Evitar dosis altas, puede ser irritante').
precauciones(boldo, 'Evitar uso prolongado y en embarazo').
precauciones(borraja, 'Evitar uso prolongado por alcaloides toxicos').
precauciones(bugambilia, 'Evitar en embarazo y dosis altas').
precauciones(brionia, 'Toxica en dosis altas, usar bajo supervision medica').
precauciones(canela, 'Evitar en personas con alergias o problemas de coagulacion').
precauciones(cedron, 'Evitar en personas con presion arterial baja').
precauciones(cardo_santo, 'Evitar contacto prolongado con los ojos, consultar medico').
precauciones(cempasuchil, 'No exceder cocimiento, maximo 3 veces al dia').
precauciones(chaparro_amargoso, 'En disenteria cronica usar como lavativa, consultar medico').
precauciones(chicalote, 'Es un poco toxica, usar con cuidado').
precauciones(chile, 'No dar a ninos, puede irritar mucosas, causar diarrea, inflamar higado o hemorroides').
precauciones(chichigua, 'Toxica si se ingiere, usar solo topicamente').
precauciones(cocolmeca, 'No usar por periodos prolongados').
precauciones(cola_de_caballo, 'Evitar en insuficiencia renal').
precauciones(colchino, 'Dosis alta toxica, usar con cuidado').
precauciones(colpachi, 'Evitar en lactancia').
precauciones(cuajiote, 'Puede provocar gastroenteritis, rectitis o enterocolitis').
precauciones(cuasia, 'Puede provocar irritacion gastrica, vomito o estupor').
precauciones(diente_de_leon, 'Puede provocar diarrea, acidez estomacal, dolor abdominal o gases').
precauciones(cilantro, 'Evitar en embarazo excesivo').
precauciones(comino, 'Evitar en embarazo').
precauciones(cuachalalate, 'No usar por periodos prolongados').
precauciones(damiana, 'Puede provocar insomnio o irritacion gastrica').
precauciones(digitaria, 'Toxica en dosis altas, usar bajo supervision medica').
precauciones(doradilla, 'Evitar en exceso por posible irritacion renal').
precauciones(epazote, 'Evitar en embarazo y ninos').
precauciones(enebro, 'Evitar en problemas renales y embarazo').
precauciones(fenogreco, 'No usar en embarazo').
precauciones(geranio, 'Posible irritacion cutanea').
precauciones(girasol, 'Evitar exceso para no causar efectos adversos').
precauciones(gingseng, 'Evitar en hipertension y embarazo').
precauciones(grama, 'Evitar en embarazo').
precauciones(guaco, 'Evitar uso prolongado').
precauciones(estafiate, 'Evitar en embarazo').
precauciones(eucalipto, 'No ingerir aceite esencial puro').
precauciones(genciana, 'Evitar en ulceras gastricas').
precauciones(guazuma, 'Evitar en caso de alergia a las hojas').
precauciones(guayacan, 'Posible interaccion con medicamentos').
precauciones(hamamelis, 'Evitar en caso de hipersensibilidad').
precauciones(helenio, 'Evitar dosis excesiva').
precauciones(jengibre, 'Evitar en hipertension').
precauciones(linaza, 'Evitar en embarazo').
precauciones(llanten, 'No precauciones especificas').
precauciones(madresilva, 'No precauciones especificas').
precauciones(maguey, 'No precauciones especificas').
precauciones(hierba_del_pollo, 'Evitar en embarazo').
precauciones(hinojo, 'Evitar en embarazo').
precauciones(jalapa, 'No precauciones especificas').
precauciones(ipecacuana, 'No precauciones especificas').
precauciones(jazmin_amarillo, 'Evitar en problemas de corazon o rinones').
precauciones(maiz, 'Moderar consumo en diabeticos, evitar exceso con diureticos').
precauciones(malva, 'No consumir en embarazo, moderar en ninos, evitar si alergia a malvaceas').
precauciones(malvavisco, 'No precauciones especificas').
precauciones(mangle, 'No usar prolongadamente, evitar en embarazo, maximo 7 dias, contraindicado en anemia').
precauciones(manzanilla, 'Evitar en alergicos a asteraceas, no usar ocular sin filtrar, moderar en embarazo, no hervir flores').
precauciones(marrubio, 'Evitar exceso por molestias estomacales').
precauciones(marihuana, 'Usar bajo supervision medica, no en menores de edad').
precauciones(mastuerzo, 'Evitar en tuberculosis avanzada').
precauciones(matarique, 'Precaucion en diabetes o problemas renales').
precauciones(palo_de_flor, 'No prolongar tratamiento').
precauciones(pinguica, 'No usar por periodos largos').
precauciones(ruibarbo, 'No usar en embarazo').
precauciones(sen, 'No usar prolongadamente').
precauciones(sanguinaria, 'Alta toxicidad si se abusa').
precauciones(sensativa, 'Evitar combinacion con sedantes').
precauciones(simonillo, 'Evitar dosis altas').
precauciones(tamarindo, 'Evitar en casos de diarrea').
precauciones(tabachin, 'Evitar en embarazo').
precauciones(taray, 'Evitar dosis excesivas').
precauciones(regaliz, 'Evitar en hipertension').
precauciones(menta, 'Evitar en reflujo gastroesofagico').
precauciones(oregano, 'Evitar dosis altas').
precauciones(pasiflora, 'No combinar con alcohol').
precauciones(pericon, 'Evitar en embarazo').
precauciones(ruda, 'Evitar en embarazo').
precauciones(salvia, 'No usar en grandes cantidades').
precauciones(retama, 'Evitar por toxicidad').
precauciones(ricino, 'Evitar semillas por toxicidad').
precauciones(rosal, 'Evitar en caso de alergia cutanea').
precauciones(nopal, 'No precauciones especificas').
precauciones(nogal, 'No precauciones especificas').
precauciones(nuez_vomica, 'No precauciones especificas').
precauciones(ocote, 'No precauciones especificas').
precauciones(ortiga, 'No precauciones especificas').
precauciones(prodigiosa, 'Evitar en embarazo').
precauciones(pirul, 'Evitar en alergia al polen').
precauciones(pulsatilla, 'Evitar por irritacion severa en piel').
precauciones(quebracho, 'Evitar en lactancia').
precauciones(quina, 'Evitar en caso de alergia').
precauciones(toloache, 'Evitar por alta toxicidad').
precauciones(tronadora, 'Evitar en hipoglucemia').
precauciones(tripa_de_judas, 'Evitar dosis excesiva').
precauciones(uva, 'Moderar en diabetes').
precauciones(romero, 'Evitar en embarazo').
precauciones(tila, 'Evitar en hipersensibilidad').
precauciones(maiz, 'Evitar en alergia al maiz').

remedio(abceso, [malva]).
remedio(abceso_hepatico, [zarzaparrilla]).
remedio(acidez_estomacal, [anis, perejil]).
remedio(acido_urico, [sanguinaria, limon, sauco]).
remedio(acne, [arnica]).
remedio(aftas, [llanten, fenogreco, zarzamora]).
remedio(agotamiento, [salvia, tilo, valeriana]).
remedio(agruras, [yerbabuena, manzanilla, jugo_de_limon, toronja]).
remedio(albuminaria, [pinguica, quina_roja, encino_rojo]).
remedio(alcoholismo, [pimiento]).
remedio(almorranas, [salvia, sauco, cola_de_caballo, sanguinaria]).
remedio(anemia, [ajenjo, quina, canela, germen_de_trigo]).
remedio(anginas, [eucalipto, cebada, salvia, borraja]).
remedio(anorexia, [ajenjo, genciana, yerbabuena]).
remedio(arterosclerosis, [limon, genciana, cardo_santo, zarzaparrilla]).
remedio(artritis, [arnica, chicalote, toronja, alcanfor, marihuana]).
remedio(asma, [eucalipto, marrubio, oregano, salvia]).
remedio(atonia_estomacal, [lupulo, eucalipto, cuasia]).
remedio(bazo, [uva, cerezo]).
remedio(boca, [malva, rosal, limon, salvia]).
remedio(bronquitis, [eucalipto, borraja, gordolobo, tilo, rabano]).
remedio(caida_cabello, [ortiga, espinosilla, marrubio, romero]).
remedio(bronconeumonia, [gordolobo, eucalipto, ipecacuana, mostaza]).
remedio(caries, [hiedra, cola_de_caballo]).
remedio(caspa, [ortiga, limon, romero]).
remedio(cancer_utero, [cuachalalate, llanten, siempreviva]).
remedio(cancer, [marihuana]).
remedio(glaucoma, [marihuana]).
remedio(males_ojos, [marihuana]).
remedio(ciatica, [mastuerzo, higuera, sauco]).
remedio(circulacion, [toronjil, sanguinaria, salvia, hamamelis]).
remedio(higado_colitis, [manzanilla]).
remedio(higado_bilis, [lechuga, tila]).
remedio(higado_ictericia, [papaloquelite, achicoria, berros, llanten]).
remedio(hipertension_alta, [ajo, esparrago, alpiste, muerdago]).
remedio(hipertension_baja, [miel, nuez_de_kola, crategus, acedera]).
remedio(hipo, [anis, hinojo, tila, valeriana]).
remedio(insomnio, [pasiflora, azahar, menta, manzanilla, lechuga, tila]).
remedio(intestino, [gencina, melisa]).
remedio(impotencia_sexual,[yohimbo,daniana, nuez_vomica, aguacate]).
remedio(jaqueca,[manzanilla,aconito,valeriana,tila,chicalote]).
remedio(lactancia,[hinojo,anis,menta,perejil,zanahoria]).
remedio(laringitis,[aconito,borraja,cebolla,rosa,benjui,encino]).
remedio(leucorrea,[encina,zarzaparrilla,pino,enebro,genciana]).
remedio(lombrices,[ajenjo,ajo,cebolla,brionia,aguacate,papaya,epazote]).
remedio(lumbago,[avena,cebada,tomillo,vernena]).
remedio(llagas,[fenogreco,eucalipto,llanten,sanguinaria]).
remedio(malaria,[quina,girasol,eucalipto,cardo_santo]).
remedio(menopausia,[azahar,hamamelis,tila,quina_roja]).
remedio(menstruacion,[azafran,hamamelis,belladana,anis_estrella,ruda,ajenjo,manzanilla,apio,hisopo,quina_amarilla,sabina,artemisa]).
remedio(metorragia,[hamamelis,zoapatle,perejil,cuerecillo_centena]).
remedio(muelas,[clavo,hiedra]).
remedio(hemorragia_nazal,[ortiga,cola_de_caballo,ruda,eucalipto]).
remedio(nauseas,[anis,ajenjo,menta,salvia]).
remedio(neuralgias,[manzanilla,menta,valeriana,boldo]).
remedio(neurastenia,[pasiflora,te_negro,mate,valeriana]).
remedio(nefritis,[linaza,grama_cebada,llanten,doradilla,esparrago,ruda]).
remedio(obesidad,[toronjil, marrudoro, malca, limon, esparrago]).
remedio(oidos,[boldo, aceite_de_oliva, llante, hiedra]).
remedio(conjuntivitis,[manzanilla, limon, llante, salvia, ruda, rosal]).
remedio(irritacion_ojo,[manzanilla, limon, llante, salvia, ruda, rosal, cardo_santo]).
remedio(nubes_ojos, [cardo_santo]).
remedio(paludismo,[ajenjo, quina]).
remedio(pecas,[berro, benciana, rabano, papaya]).
remedio(pies_olorosos,[laurel, encina]).
remedio(piquetes_de_abeja,[miel,perejil,cebolla,puerro]).
remedio(piquetes_de_arania,[fresno,ipecacuana]).
remedio(piquetes_de_mosco,[alcanfor, perejil, hamamelis]).
remedio(piquetes_de_vibora,[anagalida]).
remedio(pleuresia,[jengibre, linaza, cardo_santo, girasol]).
remedio(piorrea,[ipecacuana]).
remedio(prostata,[cola_de_caballo]).
remedio(pulmonia,[eucalipto, ocote, gordolobo, borraja, sauco]).
remedio(quemaduras,[linaza, cebolla, hiedra, gordolobo]).
remedio(raquitismo,[nogal]).
remedio(reumatismo,[ajo, apio, borrajal, gordolobo, pino, romero, fanguinaria, marrubio, tabaco, marihuana, matarique]).
remedio(riniones,[cabellos_de_elote, cola_de_caballo, apio, matarique]).
remedio(diabetes, [matarique]).
remedio(miocarditis, [digitaria]).
remedio(astenia, [digitaria]).
remedio(epilepsia, [digitaria]).
remedio(ronquera,[eucalipto, pino, gordolobo]).
remedio(sabaniones,[ajo, cebolla]).
remedio(mal_de_san_vito,[estafiare, valeriana]).
remedio(sarampion,[borraja, ortiga, sauco]).
remedio(sarna,[ajo,alcanfor,menta,tomillo,romero]).
remedio(sarpullido, [encina, salvia, tila]).
remedio(sed, [limon, tamarindo, pirul]).
remedio(solitaria, [semilla_de_calabaza, granado, coquito_de_aceite, helecho_macho]).
remedio(sudoracion_exesiva, [encina]).
remedio(tifoaidea,[alcanfor, borraja, quina, canela, romero, salvia]).
remedio(tinia,[berro, tila, tamarindo, salvia]).
remedio(tos, [eucalipto, capulin, cedron, salvia, malva, marrubio]).
remedio(tos_ferina,[gelsemio, quina, rabano, videta]).
remedio(tuberculosis,[mastuerzo, berro, ajo, eucalipto, pirul, pino, roble, platano_morado]).
remedio(ulcera,[cuachalalate, sanginaria, cola_de_caballo, girasol]).
remedio(urticaria,[limon, ruibarbo]).
remedio(varices,[hamamelis, castanio_de_indias, llanten, taranjil]).
remedio(vejiga,[apio, cipres, cola_de_caballo, ortiga, malva]).
remedio(verrugas,[leche_de_higuera, cebolla, nogal]).
remedio(vertigos,[albhaca, espino]).
remedio(vomitos,[menta, tila, marrubio, valeriana, salvia]).
remedio(voz,[cilantro, ajo, limon, pino]).
remedio(carencia_de_vitaminas,[alfalfa, espinacas, acelga, berro, cebolla, limon, zanahoria, aceite_de_bacalao]).
remedio(infeccion_urinaria, [abrojo]).
remedio(cistitis, [abrojo]).
remedio(infeccion_rinon, [abrojo]).
remedio(neuralgia, [aconito, manzanilla, menta, valeriana, boldo]).
remedio(fiebre, [aconito]).
remedio(reumatismo, [aconito, ajo, apio, borrajal, gordolobo, pino, romero, sanguinaria, marrubio, tabaco, marihuana, matarique]).
remedio(insomnio, [adormidera, pasiflora, azahar, menta, manzanilla, lechuga, tila]).
remedio(dolor, [adormidera]).
remedio(ansiedad, [adormidera]).
remedio(enfermedades_respiratorias, [ahuehuete]).
remedio(infecciones_piel, [ahuehuete]).
remedio(parasitos_intestinales, [ajenjo, ajo, cebolla, brionia, aguacate, papaya, epazote]).
remedio(problemas_digestivos, [aguacate, ajenjo]).
remedio(debilidad_general, [ajenjo]).
remedio(reumas, [ajo]).
remedio(sarna, [ajo, alcanfor, menta, tomillo, romero]).
remedio(tina, [ajo]).
remedio(callos, [ajo]).
remedio(lombrices, [ajo, ajenjo, cebolla, brionia, aguacate, papaya, epazote]).
remedio(alopecia, [albahaca]).
remedio(diabetes, [alcachofa, matarique]).
remedio(anemia, [alcachofa, ajenjo, quina, canela, germen_de_trigo]).
remedio(gota, [alcanfor]).
remedio(tifoidea, [alcanfor, borraja, quina, canela, romero, salvia]).
remedio(piquetes_mosco, [alcanfor, perejil, hamamelis]).
remedio(arteriosclerosis, [alcanfor, limon, genciana, cardo_santo, zarzaparrilla]).
remedio(colitis_leve, [anis]).
remedio(flatulencias, [anis]).
remedio(colicos, [anis, belladona]).
remedio(pulmones, [anacahuite]).
remedio(resfriado, [anacahuite]).
remedio(golpes, [arnica]).
remedio(torceduras, [arnica]).
remedio(moretones, [arnica]).
remedio(espasmos, [belladona]).
remedio(dolores_menstruales, [belladona, azafran, hamamelis, anis_estrella, ruda, ajenjo, manzanilla, apio, hisopo, quina_amarilla, sabina, artemisa]).
remedio(parkinson, [belladona]).
remedio(diviesos, [amate]).
remedio(solitaria, [amate, semilla_de_calabaza, granado, coquito_de_aceite, helecho_macho]).
remedio(infecciones_leves, [amate]).
remedio(diarrea, [amapola_amarilla]).
remedio(ansiedad_leve, [amapola_amarilla]).
remedio(verrugas, [barbasco, leche_de_higuera, cebolla, nogal]).
remedio(anticonceptivo, [barbasco]).
remedio(parasitos_intestinales, [cempasuchil, ajenjo, ajo, cebolla, brionia, aguacate, papaya, epazote]).
remedio(tumores, [cempasuchil]).
remedio(disenteria_amebiana, [chaparro_amargoso]).
remedio(flujo, [chaparro_amargoso]).
remedio(hemorragias_internas, [chaparro_amargoso]).
remedio(tosferina, [chicalote]).
remedio(colicos_hepaticos, [chicalote, manzanilla, simonillo]).
remedio(colicos_renales, [chicalote]).
remedio(colicos_intestinales, [chicalote]).
remedio(carnosidad_ojos, [chicalote]).
remedio(retencion_liquidos, [cola_de_caballo]).
remedio(calculos_renales, [cola_de_caballo]).
remedio(dolor_estomacal, [colchino]).
remedio(sarampion, [colpachi]).
remedio(afecciones_de_la_piel, [colpachi]).
remedio(anasarca, [cuajiote]).
remedio(estrenimiento_cronico, [cuajiote]).
remedio(dolor_corporal, [cuasia]).
remedio(migrana, [cuasia]).
remedio(dolor_de_estomago, [cuasia]).
remedio(hipersexualidad, [damiana]).
remedio(alcoholismo, [damiana]).
remedio(nefritis, [damiana]).
remedio(orquitis, [damiana]).
remedio(males_de_la_vejiga, [damiana]).
remedio(acumulacion_de_toxinas, [diente_de_leon]).
remedio(miocarditis, [digitaria]).
remedio(astenia, [digitaria]).
remedio(epilepsia, [digitaria, chicalote]).
remedio(leucorrea, [enebro]).
remedio(colesterol, [fenogreco]).
remedio(estres, [geranio]).
remedio(presion_alta, [girasol]).
remedio(cistitis, [grama, abrojo]).
remedio(alergia, [guaco]).
remedio(vitiligo, [guaco]).
remedio(disenteria, [granado, chaparro_amargoso]).
remedio(inflamacion_intestinal, [guazuma]).
remedio(tuberculosis, [guayacan]).
remedio(sifilis, [guayacan]).
remedio(varices, [hamamelis]).
remedio(tos_ferina, [helenio]).
remedio(males_estomacales, [linaza]).
remedio(heridas, [linaza]).
remedio(abscesos, [linaza]).
remedio(conjuntivitis, [llanten]).
remedio(pequenas_infecciones, [llanten]).
remedio(enterocolitis, [llanten]).
remedio(infecciones_garganta, [madreselva]).
remedio(llagas, [maguey]).
remedio(infecciones_piel, [maguey]).
remedio(hemorragia, [hierba_del_pollo]).
remedio(obstruccion_mucosa_pecho, [hinojo]).
remedio(apoplejia, [jalapa]).
remedio(congestion_cerebral, [jalapa]).
remedio(dolores_de_cabeza, [jazmin_amarillo]).
remedio(reuma, [jazmin_amarillo]).
remedio(espasmos, [jazmin_amarillo]).
remedio(asma_bronquial, [jazmin_amarillo]).
remedio(menstruacion_dolorosa, [jazmin_amarillo]).
remedio(irritacion_garganta, [malva]).
remedio(problemas_piel, [malva]).
remedio(inflamacion_bucal, [malva]).
remedio(gastritis, [malvavisco]).
remedio(faringitis, [malvavisco]).
remedio(ciatica, [malvavisco, mastuerzo]).
remedio(leishmaniasis, [mangle]).
remedio(diarrea_cronica, [mangle]).
remedio(hepatitis, [mangle]).
remedio(colico_menstrual, [manzanilla]).
remedio(asma, [marrubio]).
remedio(caida_cabello, [marrubio]).
remedio(obesidad, [marrubio]).
remedio(vomitos, [marrubio, menta]).
remedio(cancer, [marihuana]).
remedio(glaucoma, [marihuana]).
remedio(males_ojos, [marihuana]).
remedio(rinones_adoloridos, [matarique]).
remedio(dolor_de_cabeza, [palo_de_flor]).
remedio(infeccion_urinaria, [pinguica]).
remedio(rinones, [pinguica]).
remedio(sudoracion_excesiva, [salvia]).
remedio(problemas_respiratorios, [sanguinaria]).
remedio(inflamacion_ojos, [taray]).
remedio(escrofulosis, [nogal]).
remedio(fiebres_malignas, [nuez_vomica]).
remedio(lombrices_intestinales, [nuez_vomica]).
remedio(inflamaciones_intestinales, [quebracho]).
remedio(flujo, [quebracho]).
remedio(afecciones_del_rinon, [quebracho]).
remedio(tetano, [quina]).
remedio(epilepsia, [quina]).
remedio(eclampsia, [quina]).
remedio(gonorrea, [pirul]).
remedio(enfermedades_venereas, [pulsatilla]).
remedio(jaquecas_neuronales, [pulsatilla]).
remedio(cirrosis_hepatica, [prodigiosa]).
remedio(ictericia, [prodigiosa]).
remedio(dolor_estomacal, [tripa_de_judas]).
remedio(gripe, [tripa_de_judas]).

img('portada', 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgportada.jpg').
img(abrojo, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/abrojo.jpg').
img(acacia, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/acacia.jpg').
img(acanto, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/acanto.jpg').
img(aceitilla, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/aceitilla.jpg').
img(achicoria, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/achicoria.jpg').
img(amapola_amarilla, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/amapolaAmarilla.jpg').
img(amate, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/amate.jpg').
img(anis, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/anis.jpg').
img(brionia, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgbrionia.jpg').
img(canela, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgcanela.jpg').
img(cardo_santo, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgcardo.jpg').
img(cedron, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgcedron.jpg').
img(digitaria, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgdigitaria.jpg').
img(doradilla, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgdoradilla.jpg').
img(enebro, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgenebro.jpg').
img(epazote, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgepazote.jpg').
img(marihuana, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgmarihuana.jpg').
img(marrubio, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgmarrubio.jpg').
img(mastuerzo, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgmastuerzo.jpg').
img(matarique, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgmatarique.jpg').
img(menta, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgmenta.jpg').
img(ruda, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Ruda.jpg').
img(ruibarbo, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Ruibarbo.jpg').
img(salvia, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Salvia.jpg').
img(sanguinaria, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Sanguinaria.jpg').
img(sen, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Sen.jpg').
img(sensitiva, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Sensitiva.jpeg').
img(sensativa, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Sensitiva.jpeg').
img(simonillo, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Simonillo.jpg').
img(tamarindo, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Tamarindo.jpeg').
img(noimg, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgnoimg.jpg').
img(aconito, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Aconiato.jpg').
img(adormidera, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Adormidera.jpg').
img(ahuehuete, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Ahuehuete.jpg').
img(ajo, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/ajo.jpeg').
img(albahaca, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/albahaca.jpeg').
img(alcachofa, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/alcachofa.jpeg').
img(anacahuite, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/anacahuite.jpg').
img(barbasco, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Barbasco.jpeg').
img(bugambilia, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/imgbugambilia.jpg').
img(cempasuchil, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Cempasuchil.jpg').
img(chaparro_amargoso, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Chaparro_amargoso.jpg').
img(chicalote, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Chicalote.jpg').
img(chile, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Chile.jpg').
img(chichigua, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/chichigua.jpg').
img(cocolmeca, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/cocolmeca.jpg').
img(cola_de_caballo, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/colacaballo.jpeg').
img(colchino, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Colchino.png').
img(colpachi, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Colpachi.jpg').
img(cuajiote, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Cuajiote.jpg').
img(cuasia, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Cuasia.jpg').
img(diente_de_leon, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Diente_De_Leon.jpg').
img(fenogreco, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/fenogreco.jpg').
img(geranio, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/geranio.jpg').
img(girasol, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/girasol.jpg').
img(gingseng, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/gingseng.jpg').
img(grama, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/grama.jpg').
img(guaco, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/guaco.jpg').
img(guazuma, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/guazuma.jpeg').
img(guayacan, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/guayacan.jpg').
img(hamamelis, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/hamamelis.jpeg').
img(helenio, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/helenio.jpg').
img(madresilva, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Madreselva.jpg').
img(maguey, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Maguey.jpg').
img(hierba_del_pollo, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Hierba_del_pollo.jpg').
img(jalapa, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Jalapa.jpg').
img(ipecacuana, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Ipecacuana.jpg').
img(jazmin_amarillo, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Jazmin_Amarillo.jpg').
img(malva, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/malva.jpeg').
img(malvavisco, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/mlavavisco.jpg').
img(mangle, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/mangle.jpg').
img(palo_de_flor, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/palo_de_flor.jpeg').
img(pinguica, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/pinguica.jpeg').
img(tabachin, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Tabachin.jpg').
img(taray, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Taray.jpg').
img(regaliz, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/regaliz.jpg').
img(retama, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/retama.jpg').
img(ricino, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/ricino.jpg').
img(rosal, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/rosal.jpg').
img(nogal, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/nogal.jpg').
img(nuez_vomica, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/nuez_vomica.jpg').
img(ocote, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/Ocote.jpg').
img(ortiga, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/ortiga.jpg').
img(prodigiosa, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/prodigiosa.jpg').
img(pirul, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/pirul.jpg').
img(pulsatilla, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/pulsatilla.jpg').
img(quebracho, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/quebracho.jpg').
img(quina, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/quina.jpg').
img(toloache, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/toloache.jpeg').
img(tronadora, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/tronadora.jpeg').
img(tripa_de_judas, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/tripa_de_judas.jpeg').
img(uva, 'C:/Users/Kikin/OneDrive/Desktop/Proyecto Final Prolog/PLFE/imgs/uva.jpeg').

medicamento_Planta(sanguinaria,'enjuague_bucal').
medicamento_Planta(tamarindo,'laxantes').
medicamento_Planta(amapola_amarilla,'suplementos_del_suenio').
medicamento_Planta(anis,'anticolicos').
medicamento_Planta(ruda,'fitoterapeuticos').
medicamento_Planta(salvia,'pastillas_garganta').
medicamento_Planta(sen,'laxantes').

obtener_remedios(Remedios) :-
    findall(Remedio, remedio(Remedio, _), Remedios).

metodo_preparacion(Planta, Metodo, Descripcion) :-
    planta_preparacion(Planta, Metodo),
    preparacion(Metodo, Descripcion).

mostrar_informacion_planta(Planta) :-
    (   hierba_medicinal(Planta, Informacion)
    ->  atomics_to_string(Informacion, ', ', InfoString),
        metodo_preparacion(Planta, Metodo, MetodoDescripcion),
        (   como_ingerir(Planta, ComoIngerir)
        ->  true
        ;   ComoIngerir = 'No disponible'
        ),
        (   propiedades_planta(Planta, Propiedades)
        ->  atomics_to_string(Propiedades, ', ', PropiedadesString)
        ;   PropiedadesString = 'No disponibles'
        ),
        (   elementos_planta(Planta, Elementos)
        ->  atomics_to_string(Elementos, ', ', ElementosString)
        ;   ElementosString = 'No disponibles'
        ),
        (   origen_planta(Planta, Origen)
        ->  true
        ;   Origen = 'No disponible'
        ),
        (   nombre_cientifico(Planta, NombreCientifico)
        ->  true
        ;   NombreCientifico = 'No disponible'
        ),
        (   modo_tratamiento(Planta, ModoTratamiento)
        ->  true
        ;   ModoTratamiento = 'No disponible'
        ),
        (   precauciones(Planta, Precauciones)
        ->  true
        ;   Precauciones = 'No disponibles'
        ),
        (   img(Planta, RutaPlanta),
            exists_file(RutaPlanta)
        ->  Ruta = RutaPlanta
        ;   img(noimg, RutaPredeterminada),
            Ruta = RutaPredeterminada
        ),
        new(InfoDialog, dialog('Informacion de la Planta')),
        new(PlantaLabel, label(nombre, string('Nombre de la planta: %s', Planta))),
        new(NombreCientificoLabel, label(nombre_cientifico, string('Nombre cientifico: %s', NombreCientifico))),
        new(InfoLabel, label(informacion, string('Afecciones que trata: %s', InfoString))),
        new(MetodoLabel, label(metodo, string('Metodo de preparacion: %s', Metodo))),
        new(DescripcionLabel, label(descripcion, string('Preparacion: %s', MetodoDescripcion))),
        new(IngerirLabel, label(ingerir, string('Como ingerir: %s', ComoIngerir))),
        new(ModoTratamientoLabel, label(modo_tratamiento, string('Modo de tratamiento: %s', ModoTratamiento))),
        new(PrecaucionesLabel, label(precauciones, string('Precauciones: %s', Precauciones))),
        new(PropiedadesLabel, label(propiedades, string('Propiedades: %s', PropiedadesString))),
        new(ElementosLabel, label(elementos, string('Elementos: %s', ElementosString))),
        new(OrigenLabel, label(origen, string('Origen: %s', Origen))),
        new(LblImagen,label(string(''))),
        send(InfoDialog, display, PlantaLabel),
        send(InfoDialog, append, NombreCientificoLabel),
        send(InfoDialog, append, InfoLabel),
        send(InfoDialog, append, MetodoLabel),
        send(InfoDialog, append, DescripcionLabel),
        send(InfoDialog, append, IngerirLabel),
        send(InfoDialog, append, ModoTratamientoLabel),
        send(InfoDialog, append, PrecaucionesLabel),
        send(InfoDialog, append, PropiedadesLabel),
        send(InfoDialog, append, ElementosLabel),
        send(InfoDialog, append, OrigenLabel),
        send(InfoDialog, append, button(ok, message(InfoDialog, destroy))),
        send(InfoDialog, append, LblImagen),
        mostrar(Ruta,InfoDialog,LblImagen),
        send(InfoDialog, open_centered)
    ;   new(ErrorDialog, dialog('Error')),
        send(ErrorDialog, append, label(error, 'Planta no encontrada')),
        send(ErrorDialog, display, button(ok, message(ErrorDialog, destroy))),
        send(ErrorDialog, open_centered)
    ).

buscar_planta_dialogo :-
    new(Dialog, dialog('Buscar Planta')),
    new(NombrePlanta, text_item('Nombre de la Planta')),
    send(Dialog, append, NombrePlanta),
    send(Dialog, append, button(buscar, message(@prolog, buscar_planta, Dialog, NombrePlanta?selection))),
    send(Dialog, append, button(cancelar, message(Dialog, destroy))),
    send(Dialog, open_centered).

buscar_planta(Dialog, Planta) :-
    (   hierba_medicinal(Planta, _)
    ->  send(Dialog, destroy),
        mostrar_informacion_planta(Planta)
    ;   new(ErrorDialog, dialog('Error')),
        send(ErrorDialog, append, label(error, 'Planta no encontrada')),
        send(ErrorDialog, append, button(ok, message(ErrorDialog, destroy))),
        send(ErrorDialog, open_centered)
    ).

mostrar_informacion_remedio(Remedio) :-
    (   remedio(Remedio, Plantas)
    ->  atomics_to_string(Plantas, ', ', PlantasString),
        new(InfoDialog, dialog('Informacion de la enfermedad.')),
        new(RemedioLabel, label(nombre, string('Enfermedad: %s', Remedio))),
        new(PlantasLabel, label(plantas, string('Plantas que lo tratan: %s', PlantasString))),
        send(InfoDialog, append, RemedioLabel),
        send(InfoDialog, append, PlantasLabel),
        send(InfoDialog, append, button(ok, message(InfoDialog, destroy))),
        send(InfoDialog, open_centered)
    ;   new(ErrorDialog, dialog('Error')),
        send(ErrorDialog, append, label(error, 'Enfermedad no encontrada')),
        send(ErrorDialog, append, button(ok, message(ErrorDialog, destroy))),
        send(ErrorDialog, open_centered)
    ).

buscar_remedio(Dialog, Remedio) :-
    (   remedio(Remedio, _)
    ->  send(Dialog, destroy),
        mostrar_informacion_remedio(Remedio)
    ;   new(ErrorDialog, dialog('Error')),
        send(ErrorDialog, append, label(error, 'Padecimiento no encontrado')),
        send(ErrorDialog, append, button(ok, message(ErrorDialog, destroy))),
        send(ErrorDialog, open_centered)
    ).

mostrar_lista_enfermedades :-
    new(Dialog, dialog('Listado de Padecimientos')),
    findall(Enfermedad, remedio(Enfermedad, _), Enfermedades),
    new(ListaEnfermedades, list_browser),
    forall(member(Enfermedad, Enfermedades),
        send(ListaEnfermedades, append, Enfermedad)),
    send(Dialog, append, new(_, text('Lista de Padecimientos', center, bold))),
    send(Dialog, append, ListaEnfermedades),
    send(Dialog, append, button(cerrar, message(Dialog, destroy))),
    send(Dialog, open_centered).

mostrar_plantas_dialogo :-
    new(Dialog, dialog('Listado de Plantas')),
    findall(Planta, hierba_medicinal(Planta, _), Plantas),
    new(ListaPlantas, list_browser),
    forall(member(Planta, Plantas),
        send(ListaPlantas, append, Planta)),
    send(Dialog, append, new(_, text('Lista de Plantas', center, bold))),
    send(Dialog, append, ListaPlantas),
    send(Dialog, append, button(cerrar, message(Dialog, destroy))),
    send(Dialog, open_centered).

inicio :-
    new(D, dialog('El Yerberito', size(320, 200))),
    new(MenuBar, menu_bar),
    new(FileMenuPlantas, popup('Plantas')),
    forall(hierba_medicinal(Planta, _), 
           send(FileMenuPlantas, append, menu_item(Planta, message(@prolog, mostrar_informacion_planta, Planta)))),
    new(FileMenuEnfermedades, popup('Padecimientos')),
    new(FileMenuMedicamentos, popup('Medicamentos')),
    forall(medicamento_Planta(Planta, Medicamento), 
           send(FileMenuMedicamentos, append, menu_item(Medicamento, message(@prolog, mostrar_informacion_planta, Planta)))),
    new(ItemEnfermedades, menu_item('Lista de Padecimientos',message(@prolog, mostrar_lista_enfermedades))),
    send(FileMenuEnfermedades, append, ItemEnfermedades),
    send(MenuBar, append, FileMenuPlantas),
    send(MenuBar, append, FileMenuEnfermedades),
    send(MenuBar, append, FileMenuMedicamentos),
    send(D, append, MenuBar),
    new(BtnBuscarPlanta, button('Buscar Planta', message(@prolog, buscar_planta_dialogo))),
    new(BuscarPadecimientoButton, button('Buscar Padecimiento', message(@prolog, buscar_remedio_dialogo))),
    new(BtnBotiquin, button('Mini Botiquin', message(@prolog, mostrar_plantas_botiquin_dialogo))),
    new(BtnMostrarPlantas, button('Mostrar Plantas', message(@prolog, mostrar_plantas_dialogo))),
    new(BtnMostrarEnfermedades, button('Mostrar Enfermedades', message(@prolog, mostrar_lista_enfermedades))),
    send(D, append, BtnBuscarPlanta),
    send(D, append, BuscarPadecimientoButton),
    send(D, append, BtnBotiquin),
    send(D, append, BtnMostrarPlantas),
    send(D, append, BtnMostrarEnfermedades),
    img('portada',Ruta),
    mostrar(Ruta,D,BtnMostrarEnfermedades),
    send(D, open_centered).
:-inicio.

button1_action :-
    buscar_planta_dialogo.

mostrar_plantas_botiquin_dialogo :-
    new(Dialog, dialog('Mini Botiquin de Plantas')),
    findall(Planta, hierba_botiquin(Planta), Plantas),
    new(ListaPlantas, list_browser),
    forall(member(Planta, Plantas),
        send(ListaPlantas, append, Planta)),
    send(Dialog, append, new(_, text('Mini Botiquin de Plantas', center, bold))),
    send(Dialog, append, ListaPlantas),
    send(Dialog, append, button(cerrar, message(Dialog, destroy))),
    send(Dialog, open_centered).

buscar_remedio_dialogo :-
    new(Dialog, dialog('Buscar Padecimiento')),
    new(NombreRemedio, text_item('Padecimiento a buscar')),
    send(Dialog, append, NombreRemedio),
    send(Dialog, append, button(buscar, message(@prolog, buscar_remedio, Dialog, NombreRemedio?selection))),
    send(Dialog, append, button(cancelar, message(Dialog, destroy))),
    send(Dialog, open_centered).


