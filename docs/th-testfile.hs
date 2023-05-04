let ( computeState_0,
      run_1
      ) =
        ( \w_2 -> do
            let size_3 = Data.Map.Internal.size storesCode_4
                storesCode_4 =
                  Data.Map.Internal.insert
                    1
                    ( Unsafe.Coerce.unsafeCoerce
                        GHC.Base.. ( \( TestSimulations.World
                                          _
                                          v_5
                                        ) -> v_5
                                   )
                    )
                    ( Data.Map.Internal.insert
                        0
                        ( Unsafe.Coerce.unsafeCoerce
                            GHC.Base.. ( \( TestSimulations.World
                                              p_6
                                              _
                                            ) -> p_6
                                       )
                        )
                        Data.Map.Internal.empty
                    )
                runtimeStores_7 = Data.Vector.generate size_3 GHC.Base.$ (\i_8 -> (storesCode_4 Data.Map.Internal.! i_8) w_2)
            Qecs.Compile.Environment.WorldEnvironment runtimeStores_7 Data.Functor.<$> Qecs.Entity.createEntityStore,
          \worldEnvironment_9 ->
            Qecs.Compile.Environment.runExecutionM worldEnvironment_9
              GHC.Base.. ( ( ( ( ( GHC.Base.pure
                                     GHC.Base.. GHC.Base.const
                                       [ ( TestSimulations.Position 20 20,
                                           TestSimulations.Velocity 0 0
                                         ),
                                         ( TestSimulations.Position 20 20,
                                           TestSimulations.Velocity 10 10
                                         )
                                       ]
                                 )
                                   Control.Monad.>=> ( \bundles_10 -> do
                                                         write'_11 <- do
                                                           writeX'_12 <- do
                                                             s_13 <- do
                                                               runtimeStores_14 <- Qecs.Compile.Environment.getRuntimeStores
                                                               GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_14 Data.Vector.! 0)
                                                             GHC.Base.pure GHC.Base.$ (\entity_15 v_16 -> Control.Monad.IO.Class.liftIO GHC.Base.$ (\(Qecs.Store.Map.MapStore ref_17) entity_18 value_19 -> Data.IORef.modifyIORef ref_17 GHC.Base.$ Data.Map.Strict.Internal.insert entity_18 value_19) s_13 entity_15 v_16)
                                                           writeY'_20 <- do
                                                             s_21 <- do
                                                               runtimeStores_22 <- Qecs.Compile.Environment.getRuntimeStores
                                                               GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_22 Data.Vector.! 1)
                                                             GHC.Base.pure GHC.Base.$ (\entity_23 v_24 -> Control.Monad.IO.Class.liftIO GHC.Base.$ (\(Qecs.Store.Map.MapStore ref_25) entity_26 value_27 -> Data.IORef.modifyIORef ref_25 GHC.Base.$ Data.Map.Strict.Internal.insert entity_26 value_27) s_21 entity_23 v_24)
                                                           GHC.Base.pure GHC.Base.$
                                                             ( \e_28 z_29 ->
                                                                 let ( x_30,
                                                                       y_31
                                                                       ) = GHC.Base.id z_29
                                                                  in writeX'_12 e_28 x_30 GHC.Base.*> writeY'_20 e_28 y_31
                                                             )
                                                         entityStore_32 <- Qecs.Compile.Environment.getEntityStore
                                                         Data.Traversable.for bundles_10 GHC.Base.$
                                                           ( \bundle_33 ->
                                                               Control.Monad.IO.Class.liftIO GHC.Base.$
                                                                 ( do
                                                                     entity_34 <- Qecs.Entity.nextEntity entityStore_32
                                                                     write'_11 entity_34 bundle_33
                                                                     GHC.Base.pure entity_34
                                                                 )
                                                           )
                                                     )
                               )
                                 Control.Monad.>=> ( \i_35 -> do
                                                       iterate'_36 <- do
                                                         membersX'_37 <-
                                                           ( do
                                                               runtimeStores_38 <- Qecs.Compile.Environment.getRuntimeStores
                                                               GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_38 Data.Vector.! 0)
                                                             )
                                                             GHC.Base.>>= (Control.Monad.IO.Class.liftIO GHC.Base.. (\(Qecs.Store.Map.MapStore ref_39) -> Data.Map.Internal.size Data.Functor.<$> GHC.IORef.readIORef ref_39))
                                                         membersY'_40 <-
                                                           ( do
                                                               runtimeStores_41 <- Qecs.Compile.Environment.getRuntimeStores
                                                               GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_41 Data.Vector.! 1)
                                                             )
                                                             GHC.Base.>>= (Control.Monad.IO.Class.liftIO GHC.Base.. (\(Qecs.Store.Map.MapStore ref_42) -> Data.Map.Internal.size Data.Functor.<$> GHC.IORef.readIORef ref_42))
                                                         if membersX'_37 GHC.Classes.< membersY'_40
                                                           then do
                                                             iterateX'_43 <- do
                                                               s_44 <- do
                                                                 runtimeStores_45 <- Qecs.Compile.Environment.getRuntimeStores
                                                                 GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_45 Data.Vector.! 0)
                                                               GHC.Base.pure GHC.Base.$ (\b_46 f_47 -> Control.Monad.IO.Class.liftIO GHC.Base.$ (\(Qecs.Store.Map.MapStore ref_48) acc_49 f_50 -> GHC.IORef.readIORef ref_48 GHC.Base.>>= Data.Map.Internal.foldlWithKey' (\previous_51 entity_52 a_53 -> previous_51 GHC.Base.>>= (\b_54 -> f_50 b_54 entity_52 a_53)) (GHC.Base.pure acc_49)) s_44 b_46 f_47)
                                                             hasY'_55 <- do
                                                               s_56 <- do
                                                                 runtimeStores_57 <- Qecs.Compile.Environment.getRuntimeStores
                                                                 GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_57 Data.Vector.! 1)
                                                               GHC.Base.pure GHC.Base.$ (\entity_58 -> Control.Monad.IO.Class.liftIO GHC.Base.$ (\(Qecs.Store.Map.MapStore ref_59) entity_60 -> Data.Map.Internal.member entity_60 Data.Functor.<$> GHC.IORef.readIORef ref_59) s_56 entity_58)
                                                             readY'_61 <- do
                                                               s_62 <- do
                                                                 runtimeStores_63 <- Qecs.Compile.Environment.getRuntimeStores
                                                                 GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_63 Data.Vector.! 1)
                                                               GHC.Base.pure GHC.Base.$ (\entity_64 -> Control.Monad.IO.Class.liftIO GHC.Base.$ (\(Qecs.Store.Map.MapStore ref_65) entity_66 -> (Data.Map.Internal.! entity_66) Data.Functor.<$> GHC.IORef.readIORef ref_65) s_62 entity_64)
                                                             GHC.Base.pure GHC.Base.$
                                                               ( \initialB_67 f_68 ->
                                                                   iterateX'_43 initialB_67 GHC.Base.$
                                                                     ( \b_69 e_70 xValues_71 -> do
                                                                         yContained_72 <- hasY'_55 e_70
                                                                         if yContained_72
                                                                           then do
                                                                             yValues_73 <- Control.Monad.IO.Class.liftIO GHC.Base.$ readY'_61 e_70
                                                                             let zValues_74 =
                                                                                   GHC.Base.id
                                                                                     ( xValues_71,
                                                                                       yValues_73
                                                                                     )
                                                                             f_68 b_69 e_70 zValues_74
                                                                           else GHC.Base.pure b_69
                                                                     )
                                                               )
                                                           else do
                                                             iterateY'_75 <- do
                                                               s_76 <- do
                                                                 runtimeStores_77 <- Qecs.Compile.Environment.getRuntimeStores
                                                                 GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_77 Data.Vector.! 1)
                                                               GHC.Base.pure GHC.Base.$ (\b_78 f_79 -> Control.Monad.IO.Class.liftIO GHC.Base.$ (\(Qecs.Store.Map.MapStore ref_80) acc_81 f_82 -> GHC.IORef.readIORef ref_80 GHC.Base.>>= Data.Map.Internal.foldlWithKey' (\previous_83 entity_84 a_85 -> previous_83 GHC.Base.>>= (\b_86 -> f_82 b_86 entity_84 a_85)) (GHC.Base.pure acc_81)) s_76 b_78 f_79)
                                                             hasX'_87 <- do
                                                               s_88 <- do
                                                                 runtimeStores_89 <- Qecs.Compile.Environment.getRuntimeStores
                                                                 GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_89 Data.Vector.! 0)
                                                               GHC.Base.pure GHC.Base.$ (\entity_90 -> Control.Monad.IO.Class.liftIO GHC.Base.$ (\(Qecs.Store.Map.MapStore ref_91) entity_92 -> Data.Map.Internal.member entity_92 Data.Functor.<$> GHC.IORef.readIORef ref_91) s_88 entity_90)
                                                             readX'_93 <- do
                                                               s_94 <- do
                                                                 runtimeStores_95 <- Qecs.Compile.Environment.getRuntimeStores
                                                                 GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_95 Data.Vector.! 0)
                                                               GHC.Base.pure GHC.Base.$ (\entity_96 -> Control.Monad.IO.Class.liftIO GHC.Base.$ (\(Qecs.Store.Map.MapStore ref_97) entity_98 -> (Data.Map.Internal.! entity_98) Data.Functor.<$> GHC.IORef.readIORef ref_97) s_94 entity_96)
                                                             GHC.Base.pure GHC.Base.$
                                                               ( \initialB_99 f_100 ->
                                                                   iterateY'_75 initialB_99 GHC.Base.$
                                                                     ( \b_101 e_102 yValues_103 -> do
                                                                         xContained_104 <- hasX'_87 e_102
                                                                         if xContained_104
                                                                           then do
                                                                             xValues_105 <- Control.Monad.IO.Class.liftIO GHC.Base.$ readX'_93 e_102
                                                                             let zValues_106 =
                                                                                   GHC.Base.id
                                                                                     ( xValues_105,
                                                                                       yValues_103
                                                                                     )
                                                                             f_100 b_101 e_102 zValues_106
                                                                           else GHC.Base.pure b_101
                                                                     )
                                                               )
                                                       write'_107 <- do
                                                         s_108 <- do
                                                           runtimeStores_109 <- Qecs.Compile.Environment.getRuntimeStores
                                                           GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_109 Data.Vector.! 0)
                                                         GHC.Base.pure GHC.Base.$ (\entity_110 v_111 -> Control.Monad.IO.Class.liftIO GHC.Base.$ (\(Qecs.Store.Map.MapStore ref_112) entity_113 value_114 -> Data.IORef.modifyIORef ref_112 GHC.Base.$ Data.Map.Strict.Internal.insert entity_113 value_114) s_108 entity_110 v_111)
                                                       Control.Monad.IO.Class.liftIO GHC.Base.$
                                                         ( iterate'_36 GHC.Tuple . () GHC.Base.$
                                                             ( \b_115 e_116 v_117 -> do
                                                                 let ( o_118,
                                                                       x_119
                                                                       ) =
                                                                         ( \_ i_120 ->
                                                                             ( ( \( TestSimulations.Position
                                                                                      a_121
                                                                                      b_122,
                                                                                    TestSimulations.Velocity
                                                                                      da_123
                                                                                      db_124
                                                                                    ) -> TestSimulations.Position (a_121 GHC.Num.+ da_123) (b_122 GHC.Num.+ db_124)
                                                                               )
                                                                                 i_120,
                                                                               GHC.Tuple . ()
                                                                             )
                                                                         )
                                                                           i_35
                                                                           v_117
                                                                 write'_107 e_116 o_118
                                                                 GHC.Base.pure GHC.Base.$ (\_ _ -> GHC.Tuple . ()) b_115 x_119
                                                             )
                                                         )
                                                   )
                             )
                               Control.Monad.>=> ( \i_125 -> do
                                                     iterate'_126 <- do
                                                       s_127 <- do
                                                         runtimeStores_128 <- Qecs.Compile.Environment.getRuntimeStores
                                                         GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_128 Data.Vector.! 0)
                                                       GHC.Base.pure GHC.Base.$ (\b_129 f_130 -> Control.Monad.IO.Class.liftIO GHC.Base.$ (\(Qecs.Store.Map.MapStore ref_131) acc_132 f_133 -> GHC.IORef.readIORef ref_131 GHC.Base.>>= Data.Map.Internal.foldlWithKey' (\previous_134 entity_135 a_136 -> previous_134 GHC.Base.>>= (\b_137 -> f_133 b_137 entity_135 a_136)) (GHC.Base.pure acc_132)) s_127 b_129 f_130)
                                                     write'_138 <- do
                                                       s_139 <- do
                                                         runtimeStores_140 <- Qecs.Compile.Environment.getRuntimeStores
                                                         GHC.Base.pure GHC.Base.$ Unsafe.Coerce.unsafeCoerce (runtimeStores_140 Data.Vector.! 0)
                                                       GHC.Base.pure GHC.Base.$ (\entity_141 v_142 -> Control.Monad.IO.Class.liftIO GHC.Base.$ (\(Qecs.Store.Map.MapStore ref_143) entity_144 value_145 -> Data.IORef.modifyIORef ref_143 GHC.Base.$ Data.Map.Strict.Internal.insert entity_144 value_145) s_139 entity_141 v_142)
                                                     Control.Monad.IO.Class.liftIO GHC.Base.$
                                                       ( iterate'_126 GHC.Base.mempty GHC.Base.$
                                                           ( \b_146 e_147 v_148 -> do
                                                               let ( o_149,
                                                                     x_150
                                                                     ) =
                                                                       ( \_ i_151 ->
                                                                           ( i_151,
                                                                             ( \( TestSimulations.Position
                                                                                    a_152
                                                                                    b_153
                                                                                  ) -> Data.Semigroup.Internal.Sum a_152 GHC.Num.+ Data.Semigroup.Internal.Sum b_153
                                                                             )
                                                                               i_151
                                                                           )
                                                                       )
                                                                         i_125
                                                                         v_148
                                                               write'_138 e_147 o_149
                                                               GHC.Base.pure GHC.Base.$ (GHC.Base.<>) b_146 x_150
                                                           )
                                                       )
                                                 )
                           )
                             Control.Monad.>=> (GHC.Base.pure GHC.Base.. Data.Semigroup.Internal.getSum)
                         )
        )
 in run_1